# OncoClinical plausibility verifications
# named as ocv_pl<number>.

ocv_pl1 <- function(project, connection) {
  first_pd <-
    select_tbl("first_progression", "pd_date", .connection = connection) |>
    dplyr::filter(
      .data$project == .env$project,
      !is.na(.data$pd_date)
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      pd_date = lubridate::ymd(.data$pd_date)
    )

  ttms <-
    select_tbl(
      "antineoplasic_therapy",
      "ttm_met_line_num",
      "ttm_startdate",
      .connection = connection
    ) |>
    dplyr::filter(
      .data$project == .env$project,
      !is.na(.data$ttm_met_line_num),
      !is.na(.data$ttm_startdate)
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      ttm_startdate = lubridate::ymd(.data$ttm_startdate)
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

ocv_pl2 <- function(project, connection) {
  select_tbl(
    "antineoplasic_therapy",
    "ttm_startdate",
    "ttm_enddate",
    .connection = connection
  ) |>
    dplyr::filter(
      .data$project == .env$project,
      !is.na(.data$ttm_startdate),
      !is.na(.data$ttm_enddate)
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      .ok = .data$ttm_startdate <= .data$ttm_enddate
    ) |>
    filter_issues(
      "ttm_startdate ({ttm_startdate}) is after ttm_enddate ({ttm_enddate})"
    )
}

ocv_pl3 <- function(project, connection) {
  ttms <-
    select_tbl(
      "antineoplasic_therapy",
      "ttm_startdate",
      "ttm_enddate",
      .connection = connection
    ) |>
    dplyr::filter(
      .data$project == .env$project
    ) |>
    dplyr::collect()

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

ocv_pl4 <- function(project, connection) {
  ttm_and_next_ttm <-
    extract_ordered_treatment(project, connection) |>
    add_next_setting_date("ttm_startdate")

  ocv_pl4_a <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      .ok = .data$ttm_startdate <= .data$next_ttm_startdate |
        is.na(.data$next_ttm_startdate) |
        is.na(.data$ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_startdate ({ttm_startdate}) is after {next_setting} ttm_startdate ({next_ttm_startdate})"
    )
  ocv_pl4_b <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      .ok = .data$ttm_startdate != .data$next_ttm_startdate |
        is.na(.data$next_ttm_startdate) |
        is.na(.data$ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_startdate ({ttm_startdate}) is equal to {next_setting} ttm_startdate ({next_ttm_startdate})"
    )

  dplyr::bind_rows(ocv_pl4_a, ocv_pl4_b)
}

ocv_pl5 <- function(project, connection) {
  ttm_and_next_ttm <-
    extract_ordered_treatment(project, connection) |>
    add_next_setting_date("ttm_enddate")

  ocv_pl5_a <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      .ok = .data$ttm_enddate <= .data$next_ttm_enddate |
        is.na(.data$next_ttm_enddate) |
        is.na(.data$ttm_enddate)
    ) |>
    filter_issues(
      "{setting_label} ttm_enddate ({ttm_enddate}) is after {next_setting} ttm_enddate ({next_ttm_enddate})"
    )
  ocv_pl5_b <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      .ok = .data$ttm_enddate != .data$next_ttm_enddate |
        is.na(.data$next_ttm_enddate) |
        is.na(.data$ttm_enddate)
    ) |>
    filter_issues(
      "{setting_label} ttm_enddate ({ttm_enddate}) is equal to {next_setting} ttm_enddate ({next_ttm_enddate})"
    )

  dplyr::bind_rows(ocv_pl5_a, ocv_pl5_b)
}
