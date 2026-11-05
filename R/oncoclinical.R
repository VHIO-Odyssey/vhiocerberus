verify_oncoclinical_data <- function(user, password, project) {
  connection <- connect_to_vhio_database(user, password, "OncoClinical")
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  verifications <-
    oncoclinical_verifications |>
    dplyr::filter(.data[[project]] == 1) |>
    dplyr::select("verif_id", "type", "verification", "details") |>
    dplyr::mutate(
      issues = purrr::map(
        .data$verif_id,
        \(verif_id) {
          cli::cli_inform("Running {.val {verif_id}} verification...")

          result <- try(
            do.call(verif_id, list(project = project, connection = connection)),
            silent = TRUE
          )

          if (inherits(result, "try-error")) {
            cli::cli_alert_danger(
              "{.val {verif_id}} failed: {conditionMessage(attr(result, 'condition'))}"
            )
            return(NULL)
          }

          cli::cli_alert_success("{.val {verif_id}} verification completed")
          result |>
            dplyr::mutate(
              dem_sap = as.character(.data$dem_sap)
            ) |>
            dplyr::select(
              # Esto elimina la columna de eventos e instancias si todas son 0
              -dplyr::where(~ all(. == 0))
            )
        }
      )
    ) |>
    dplyr::mutate(
      # NULL means the verification failed to run
      n_issues = purrr::map_int(
        .data$issues,
        \(x) if (is.null(x)) NA_integer_ else nrow(x)
      ),
      .before = "issues"
    )

  attr(verifications, "project") <- project
  attr(verifications, "verification_datetime") <- Sys.time()
  verifications
}

report_oncoclinical_verifications <- function(
  oncoclinical_verifications,
  folder = NULL
) {
  project <- attr(oncoclinical_verifications, "project")
  verification_datetime <- attr(
    oncoclinical_verifications,
    "verification_datetime"
  )
  report_file_name <-
    stringr::str_c(
      project,
      "_oncoclinical_verifications_",
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

  sheets <- c(
    list(
      project_info = project_info,
      verifications_summary = verifications_summary
    ),
    oncoclinical_verifications |>
      dplyr::filter(.data$n_issues > 0) |>
      dplyr::select("verif_id", "issues") |>
      tibble::deframe()
  )

  # Excel sheet names must be <= 31 characters and unique
  names(sheets) <- make.unique(substr(names(sheets), 1, 31))

  summary_sheet_name <- names(sheets)[2]
  # Map each verif_id to its (possibly truncated) sheet name
  sheet_name_map <- stats::setNames(names(sheets)[-(1:2)], issue_verif_ids)

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

  # Below each verification table, add a link back to the summary
  for (verif_id in issue_verif_ids) {
    sheet_name <- sheet_name_map[[verif_id]]
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
