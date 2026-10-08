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
        ~ do.call(., list(project = project, connection = connection))
      )
    ) |>
    dplyr::mutate(
      n_issues = purrr::map_int(.data$issues, nrow),
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

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}
