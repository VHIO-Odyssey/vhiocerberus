# OncoClinical plausibility verifications
# named as ocv_pl<number>.

ocv_pl1 <- function(project, connection) {
  print("Running ocv_pl1 verification")

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
      " {ttm_met_line_num}L start ({ttm_startdate}) is before pd_date ({pd_date})"
    )
}
