#' @title Run the OncoClinical Verifications of a Project
#' @description
#'   Connects to the OncoClinical database and runs every verification (`ocv_*`)
#'   enabled for `project` in `oncoclinical_verifications`. A verification that
#'   fails to run does not stop the others: it is reported and its result is
#'   `NULL`.
#'
#'   Records are always filtered by `project` and, if `patients` is given, also
#'   by patient, so a patient present in several projects only contributes the
#'   records of `project`.
#'
#' @param project A single string with the project name. It must be one of the
#'   project columns of `oncoclinical_verifications`.
#' @param patients A vector of patient identifiers (`dem_sap`) to verify, or
#'   `NULL` (the default) to verify all the patients of the project. Patients
#'   without a record in `project` are ignored with a warning.
#' @param user,password,server Credentials and address of the SQL Server. If
#'   `NULL` (the default), they are read from the environment variables
#'   `VHIO_SQL_SERVER_USER`, `VHIO_SQL_SERVER_PASSWORD` and
#'   `VHIO_SQL_SERVER_IP`, which you can define in your `~/.Renviron`. Avoid
#'   typing the password in scripts.
#'
#' @return A tibble with one row per verification and the columns `verif_id`,
#'   `type`, `verification`, `details`, `n_issues` (`NA` if the verification
#'   failed to run) and `issues` (list-column with the tibble of issues, or
#'   `NULL` if it failed). Identifier columns whose values are all `0` are
#'   dropped from each tibble of issues. The attributes `project`,
#'   `verification_datetime` and `patients` (the unique requested patients, or
#'   `NULL` if all were verified) are used by
#'   `report_oncoclinical_verifications()`.
#'
#' @seealso [report_oncoclinical_verifications()], [oncoclinical_projects()]
#' @export
verify_oncoclinical_data <- function(
  project,
  patients = NULL,
  user = NULL,
  password = NULL,
  server = NULL
) {
  project <- rlang::arg_match0(project, oncoclinical_projects())
  credentials <- vhio_credentials(user, password, server)

  connection <- connect_to_vhio_database(
    credentials$server,
    credentials$user,
    credentials$password,
    "OncoClinical"
  )
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  oncopharma <- connect_to_vhio_database(
    credentials$server,
    credentials$user,
    credentials$password,
    "OncoPharma"
  )
  on.exit(DBI::dbDisconnect(oncopharma), add = TRUE)
  # patients = NULL: sin filtro de pacientes, se verifican todos los del proyecto
  warn_patients_not_in_project(patients, project, connection)

  context <- list(
    project = project,
    patients = patients,
    connection = connection,
    oncopharma = oncopharma
  )

  verifications <-
    oncoclinical_verifications |>
    dplyr::filter(.data[[project]] == 1) |>
    dplyr::select("verif_id", "type", "verification", "details")

  ids <- verifications$verif_id
  n_verifications <- length(ids)

  progress_id <- cli::cli_progress_bar(
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {status}",
    total = n_verifications
  )

  verifications <-
    verifications |>
    dplyr::mutate(
      issues = purrr::imap(
        .data$verif_id,
        \(verif_id, i, context) {
          status <- paste0(verif_id, " | pending: ", n_verifications - i)
          cli::cli_progress_update(
            id = progress_id,
            status = status,
            set = i - 1
          )

          result <- try(
            run_verification(verif_id, context),
            silent = TRUE
          )

          if (inherits(result, "try-error")) {
            return(NULL)
          }

          result |>
            dplyr::mutate(dem_sap = as.character(.data$dem_sap)) |>
            drop_zero_id_columns()
        },
        context
      )
    )

  cli::cli_progress_done(id = progress_id)

  verifications <-
    verifications |>
    dplyr::mutate(
      # NULL means the verification failed to run
      n_issues = purrr::map_int(
        .data$issues,
        \(x) if (is.null(x)) NA_integer_ else nrow(x)
      ),
      .before = "issues"
    )
  failed <- verifications$verif_id[is.na(verifications$n_issues)]
  n_failed <- length(failed)
  n_succeeded <- n_verifications - n_failed

  if (n_failed == 0) {
    cli::cli_alert_success(
      "{n_succeeded}/{n_verifications} verification{?s} completed."
    )
  } else {
    cli::cli_warn(c(
      "{n_succeeded} verification{?s} completed, {n_failed} failed.",
      "i" = "Failed: {.val {failed}}."
    ))
  }

  attr(verifications, "project") <- project
  attr(verifications, "verification_datetime") <- Sys.time()
  attr(verifications, "patients") <- if (is.null(patients)) {
    NULL
  } else {
    unique(patients)
  }
  verifications
}

#' @title Run a Single Verification
#' @description
#'   Calls the verification `verif_id` passing only the arguments from
#'   `context` that the function declares. If the function has `...`, it
#'   receives the whole `context`.
#'
#' @param verif_id Name of the verification function (e.g. `"ocv_pl1"`).
#' @param context Named list with everything available to verifications
#'   (`project`, `patients`, `connection`, auxiliary connections...).
#'
#' @return The result of the verification.
#' @keywords internal
#' @noRd
run_verification <- function(verif_id, context) {
  fn <- get(verif_id, mode = "function")
  declared <- rlang::fn_fmls_names(fn)

  args <- if ("..." %in% declared) {
    context
  } else {
    context[intersect(names(context), declared)]
  }

  do.call(verif_id, args)
}


#' @title Project Names Available in the Verifications
#' @description
#'   Names of the project columns of `oncoclinical_verifications`, i.e. the
#'   projects for which verifications can be run.
#'
#' @return A character vector of project names.
#'
#' @seealso [verify_oncoclinical_data()]
#' @export
#' @examples
#' oncoclinical_projects()
oncoclinical_projects <- function() {
  setdiff(
    names(oncoclinical_verifications),
    c("verif_id", "type", "verification", "details")
  )
}

#' @title Drop Identifier Columns That Are Always Zero
#' @description
#'   Removes the `event_arm` and `repeating_instance` columns (including the
#'   suffixed ones, e.g. `repeating_instance_1`) of a tibble of issues when
#'   every value is `0`, since they carry no information (the form is not
#'   repeating or has a single event). Columns with missing values, and every
#'   column of a tibble with zero rows, are kept.
#'
#' @param issues A tibble of issues as returned by `filter_issues()`.
#'
#' @return `issues` without the all-zero identifier columns.
#' @keywords internal
drop_zero_id_columns <- function(issues) {
  id_cols <- grep(
    "^(event_arm|repeating_instance)(_.*)?$",
    names(issues),
    value = TRUE
  )
  all_zero <- vapply(
    issues[id_cols],
    \(x) length(x) > 0 && !anyNA(x) && all(x == 0),
    logical(1)
  )

  dplyr::select(issues, !tidyselect::all_of(id_cols[all_zero]))
}

#' @title Warn About Patients Absent from a Project
#' @description
#'   Checks that every requested patient has a record in the `demographics`
#'   form of `project` and, if some do not, issues a warning listing them.
#'   Those patients would otherwise be silently ignored by the verifications.
#'   Nothing is queried if `patients` is `NULL`.
#'
#' @param patients A vector of patient identifiers (`dem_sap`), or `NULL`.
#' @param project A single string with the project name.
#' @param connection A `DBIConnection` object to the OncoClinical database.
#'
#' @return Invisibly, the patients not found in the project (a vector of the
#'   same type as `patients`, empty if all were found).
#' @keywords internal
warn_patients_not_in_project <- function(patients, project, connection) {
  if (is.null(patients)) {
    return(invisible(NULL))
  }

  patients <- unique(patients)

  found <-
    select_oncoclinical_tbl(
      "demographics",
      "dem_sap",
      .connection = connection
    ) |>
    dplyr::filter(.data$project == .env$project) |>
    filter_patients(patients) |>
    dplyr::pull("dem_sap")

  not_found <- patients[!patients %in% found]

  if (length(not_found) > 0) {
    cli::cli_warn(c(
      "{length(not_found)} patient{?s} not found in project {.val {project}}.",
      "x" = "{.val {not_found}}",
      "i" = "They are ignored by the verifications."
    ))
  }

  invisible(not_found)
}

#' @title Issues of All Verifications, Ordered by Patient
#' @description
#'   Stacks the issues of every verification that found any into a single
#'   tibble ordered by `dem_sap`. All columns are converted to character so
#'   that issues with different column types can be combined, and the
#'   `form` and `repeating_instance` columns are renamed with a `_1` suffix to
#'   match the `_2`, `_3` suffixed columns of verifications comparing several
#'   forms.
#'
#' @param verifications A tibble as returned by `verify_oncoclinical_data()`.
#'
#' @return A tibble with the columns `dem_sap`, `verif_id`, the form and
#'   repeating instance columns present in the issues, and `issue`.
#' @keywords internal
issues_by_sap <- function(verifications) {
  verifications |>
    dplyr::filter(!is.na(.data$n_issues), .data$n_issues > 0) |>
    dplyr::mutate(
      issues = purrr::map(
        .data$issues,
        \(x) {
          x |>
            dplyr::mutate(
              dplyr::across(tidyselect::everything(), as.character)
            ) |>
            dplyr::rename_with(
              \(name) stringr::str_c(name, "_1"),
              tidyselect::any_of(c("form", "repeating_instance"))
            )
        }
      )
    ) |>
    tidyr::unnest("issues") |>
    dplyr::arrange(.data$dem_sap) |>
    dplyr::select(
      "dem_sap",
      "verif_id",
      tidyselect::any_of(c(
        "form_1",
        "repeating_instance_1",
        "form_2",
        "repeating_instance_2",
        "form_3",
        "repeating_instance_3"
      )),
      "issue"
    )
}

#' @title Write the OncoClinical Verifications to an Excel Report
#' @description
#'   Writes the result of [verify_oncoclinical_data()] to an Excel workbook.
#'   The file is named
#'   `<project>_oncoclinical_verifications[_subset]_<YYYYmmdd_HHMM>.xlsx`, where
#'   the `_subset` suffix is only added if a subset of patients was verified.
#'
#'   The workbook contains the following sheets:
#'   * `project_info`: project, patients verified and verification date-time.
#'   * `verifications_summary`: one row per verification with its number of
#'     issues. Each non-zero count links to the sheet of that verification.
#'   * `all_issues`: all the issues of all the verifications, one row per issue
#'     and ordered by patient. Omitted if there are no issues.
#'   * One sheet per verification with issues, named after its `verif_id`, with
#'     a link back to the summary.
#'
#' @param oncoclinical_verifications The tibble returned by
#'   [verify_oncoclinical_data()]. Its attributes `project`,
#'   `verification_datetime` and `patients` are used to name the file and fill
#'   the `project_info` sheet.
#' @param folder A single string with a folder, relative to the project root
#'   (see [here::here()]), where the report is saved. `NULL` (the default)
#'   saves it in the project root. The folder must already exist.
#'
#' @return The path of the report, invisibly. An existing file with the same
#'   name is overwritten.
#'
#' @seealso [verify_oncoclinical_data()]
#' @export
report_oncoclinical_verifications <- function(
  oncoclinical_verifications,
  folder = NULL
) {
  project <- attr(oncoclinical_verifications, "project")
  verification_datetime <- attr(
    oncoclinical_verifications,
    "verification_datetime"
  )
  patients <- attr(oncoclinical_verifications, "patients")
  report_file_name <-
    stringr::str_c(
      project,
      "_oncoclinical_verifications",
      # NULL se omite: solo los informes de un subset llevan este sufijo
      if (!is.null(patients)) "_subset",
      "_",
      format(verification_datetime, "%Y%m%d_%H%M"),
      ".xlsx"
    )

  if (is.null(folder)) {
    path <- here::here(report_file_name)
  } else {
    path_components <- c(folder, report_file_name) |>
      stringr::str_c(collapse = "/")
    path <- here::here(path_components)
  }

  project_info <- dplyr::tibble(
    project = project,
    patients = if (is.null(patients)) {
      "All patients in the project"
    } else {
      stringr::str_c("Subset of ", length(patients), " patients")
    },
    verification_datetime = verification_datetime
  )

  verifications_summary <-
    oncoclinical_verifications |>
    dplyr::select(
      "verif_id",
      "type",
      "verification",
      "n_issues"
    )

  issue_verif_ids <-
    oncoclinical_verifications |>
    dplyr::filter(!is.na(.data$n_issues), .data$n_issues > 0) |>
    dplyr::pull("verif_id")

  # all_issues goes right after the summary; omitted if there are no issues
  n_fixed_sheets <- if (length(issue_verif_ids) > 0) 3 else 2

  sheets <- c(
    list(
      project_info = project_info,
      verifications_summary = verifications_summary
    ),
    if (length(issue_verif_ids) > 0) {
      list(all_issues = issues_by_sap(oncoclinical_verifications))
    },
    oncoclinical_verifications |>
      dplyr::filter(.data$n_issues > 0) |>
      dplyr::select("verif_id", "issues") |>
      tibble::deframe()
  )

  # Excel sheet names must be <= 31 characters and unique
  names(sheets) <- make.unique(substr(names(sheets), 1, 31))

  summary_sheet_name <- names(sheets)[2]
  # Map each verif_id to its (possibly truncated) sheet name
  sheet_name_map <- stats::setNames(
    names(sheets)[-seq_len(n_fixed_sheets)],
    issue_verif_ids
  )

  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9E1F2",
    border = "bottom"
  )

  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(workbook, sheet_name)
    openxlsx::writeDataTable(workbook, sheet_name, sheets[[sheet_name]])
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = seq_along(sheets[[sheet_name]]),
      widths = "auto"
    )
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
  }

  hyperlink_style <- openxlsx::createStyle(
    fontColour = "#0563C1",
    textDecoration = "underline"
  )

  # In the summary, link each n_issues > 0 to its verification sheet
  linked_rows <- which(
    !is.na(verifications_summary$n_issues) & verifications_summary$n_issues > 0
  )
  for (i in linked_rows) {
    target_sheet <- sheet_name_map[[verifications_summary$verif_id[i]]]
    openxlsx::writeFormula(
      workbook,
      summary_sheet_name,
      x = sprintf(
        'HYPERLINK("#\'%s\'!A1","%s")',
        target_sheet,
        verifications_summary$n_issues[i]
      ),
      startCol = which(names(verifications_summary) == "n_issues"),
      startRow = i + 1
    )
    openxlsx::addStyle(
      workbook,
      summary_sheet_name,
      hyperlink_style,
      rows = i + 1,
      cols = which(names(verifications_summary) == "n_issues")
    )
  }

  # Below each issues table, add a link back to the summary
  sheets_with_back_link <- names(sheets)[-(1:2)]
  for (sheet_name in sheets_with_back_link) {
    back_row <- nrow(sheets[[sheet_name]]) + 3
    openxlsx::writeFormula(
      workbook,
      sheet_name,
      x = sprintf(
        'HYPERLINK("#\'%s\'!A1","Back to summary")',
        summary_sheet_name
      ),
      startCol = 1,
      startRow = back_row
    )
    openxlsx::addStyle(
      workbook,
      sheet_name,
      hyperlink_style,
      rows = back_row,
      cols = 1
    )
  }

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}
