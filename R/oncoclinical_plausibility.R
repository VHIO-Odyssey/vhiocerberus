# OncoClinical plausibility verifications
# named as ocv_pl<number>.

ocv_pl1 <- function(project, patients, connection) {
  first_pd <-
    oncoclinical_select(
      "pd_date",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(is.na(.data$pd_date))

  ttms <-
    oncoclinical_select(
      "ttm_met_line_num",
      "ttm_startdate",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(
      dplyr::when_any(
        is.na(.data$ttm_met_line_num),
        is.na(.data$ttm_startdate)
      )
    )

  dplyr::inner_join(
    first_pd,
    ttms,
    by = c("dem_sap", "project", "event_arm"),
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      .ok = .data$pd_date <= .data$ttm_startdate
    ) |>
    filter_issues(
      "{ttm_met_line_num}L start ({ttm_startdate}) is before pd_date ({pd_date})"
    )
}

ocv_pl2 <- function(project, patients, connection) {
  oncoclinical_select(
    "ttm_startdate",
    "ttm_enddate",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::filter_out(
      dplyr::when_any(
        is.na(.data$ttm_startdate),
        is.na(.data$ttm_enddate)
      )
    ) |>
    dplyr::mutate(
      .ok = .data$ttm_startdate <= .data$ttm_enddate
    ) |>
    filter_issues(
      "ttm_startdate ({ttm_startdate}) is after ttm_enddate ({ttm_enddate})"
    )
}

ocv_pl3 <- function(project, patients, connection) {
  ttms <-
    oncoclinical_select(
      "ttm_startdate",
      "ttm_enddate",
      .project = project,
      .patients = patients,
      .connection = connection
    )

  ocv_pl3_a <-
    ttms |>
    dplyr::select(-"ttm_enddate") |>
    dplyr::filter_out(is.na(.data$ttm_startdate)) |>
    dplyr::summarise(
      n_startdate = dplyr::n(),
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        collapse = ", "
      ),
      .by = c("dem_sap", "project", "event_arm", "form", "ttm_startdate")
    ) |>
    dplyr::mutate(
      .ok = .data$n_startdate == 1
    ) |>
    filter_issues(
      "Instances {repeating_instance} have the same ttm_startdate ({ttm_startdate})"
    )

  ocv_pl3_b <-
    ttms |>
    dplyr::select(-"ttm_startdate") |>
    dplyr::filter_out(is.na(.data$ttm_enddate)) |>
    dplyr::summarise(
      n_enddate = dplyr::n(),
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        collapse = ", "
      ),
      .by = c("dem_sap", "project", "event_arm", "form", "ttm_enddate")
    ) |>
    dplyr::mutate(
      .ok = .data$n_enddate == 1
    ) |>
    filter_issues(
      "Instances {repeating_instance} have the same ttm_enddate ({ttm_enddate})"
    )

  dplyr::bind_rows(ocv_pl3_a, ocv_pl3_b) |>
    dplyr::arrange(.data$dem_sap)
}

ocv_pl4 <- function(project, patients, connection) {
  ocv_next_setting_date_order(project, patients, connection, "ttm_startdate")
}

ocv_pl5 <- function(project, patients, connection) {
  ocv_next_setting_date_order(project, patients, connection, "ttm_enddate")
}

ocv_pl6 <- function(project, patients, connection) {
  ocv_next_setting_date_order(project, patients, connection, "ttm_pddate")
}

ocv_pl7 <- function(project, patients, connection) {
  extract_ordered_treatment(project, patients, connection) |>
    add_next_setting_date("ttm_startdate") |>
    dplyr::mutate(
      repeating_instance = stringr::str_c(
        repeating_instance,
        next_instance,
        sep = ", "
      ),
      .ok = ttm_enddate <= next_ttm_startdate |
        is.na(ttm_enddate) |
        is.na(next_ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_enddate ({ttm_enddate}) is after {next_setting} ttm_startdate ({next_ttm_startdate})"
    )
}

ocv_pl8 <- function(project, patients, connection) {
  extract_ordered_treatment(project, patients, connection) |>
    add_next_setting_date("ttm_startdate") |>
    dplyr::mutate(
      repeating_instance = stringr::str_c(
        repeating_instance,
        next_instance,
        sep = ", "
      ),
      .ok = ttm_pddate <= next_ttm_startdate |
        is.na(ttm_pddate) |
        is.na(next_ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_pddate ({ttm_pddate}) is after {next_setting} ttm_startdate ({next_ttm_startdate})"
    )
}

ocv_pl9 <- function(project, patients, connection) {
  extract_ordered_treatment(project, patients, connection) |>
    dplyr::mutate(
      .ok = ttm_pddate >= ttm_startdate |
        is.na(ttm_pddate) |
        is.na(ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_pddate ({ttm_pddate}) is before ttm_startdate ({ttm_startdate})"
    )
}
