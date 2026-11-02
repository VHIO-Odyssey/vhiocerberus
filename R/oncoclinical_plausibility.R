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
      "ttm_stardate ({ttm_startdate}) is after ttm_enddate ({ttm_enddate})"
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
      repeating_instance = stringr::str_c(repeating_instance, collapse = ", "),
      .by = c("dem_sap", "project", "event_arm", "form", "ttm_startdate")
    ) |>
    dplyr::mutate(
      .ok = n_startdate == 1
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
      repeating_instance = stringr::str_c(repeating_instance, collapse = ", "),
      .by = c("dem_sap", "project", "event_arm", "form", "ttm_enddate")
    ) |>
    dplyr::mutate(
      .ok = n_enddate == 1
    ) |>
    filter_issues(
      "Instances {repeating_instance} have the same ttm_enddate ({ttm_enddate})"
    )

  dplyr::bind_rows(ocv_pl3_a, ocv_pl3_b) |>
    dplyr::arrange(.data$dem_sap)
}

#wip ocv_pl4
ocv_pl4 <- function(project, connection) {
  select_tbl(
    "antineoplasic_therapy",
    "ttm_met_line_num",
    "ttm_setting",
    "ttm_ad_int",
    "ttm_startdate",
    .connection = connection
  ) |>
    dplyr::filter(
      .data$project == .env$project
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      ttm_met_line_num2 = dplyr::if_else(
        stringr::str_detect(.data$ttm_met_line_num, "^\\d+$"),
        as.numeric(.data$ttm_met_line_num) |> suppressWarnings(),
        NA_real_
      ),
      num_order = dplyr::case_when(
        !is.na(.data$ttm_met_line_num2) ~ as.numeric(.data$ttm_met_line_num2),
        .data$ttm_setting == "Neoadjuvant" ~ -1,
        .data$ttm_setting == "Adjuvant" &
          .data$ttm_ad_int == "After primary tumor surgery" ~ 0,
        .default = NA
      ),
      setting_label = dplyr::case_when(
        .data$num_order == -1 ~ "Neoadjuvant",
        .data$num_order == 0 ~ "Adjuvant",
        !is.na(.data$ttm_met_line_num) ~ stringr::str_c(
          .data$ttm_met_line_num,
          "L"
        ),
        .default = NA_character_
      )
    ) |>
    dplyr::filter_out(
      is.na(.data$num_order) | is.na(.data$ttm_startdate)
    ) |>
    dplyr::slice_min(
      .data$ttm_startdate,
      by = c("dem_sap", "event_arm", "num_order"),
    ) |>
    dplyr::arrange(.data$dem_sap, .data$num_order) |>
    dplyr::mutate(
      next_startdate = dplyr::lead(.data$ttm_startdate),
      next_label = dplyr::lead(.data$setting_label),
      next_instance = dplyr::lead(.data$repeating_instance),
      .by = c("dem_sap", "event_arm")
    ) |>
    dplyr::mutate(
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        .data$next_instance,
        sep = ", "
      ),
      .ok = is.na(.data$next_startdate) |
        .data$ttm_startdate <= .data$next_startdate
    ) |>
    filter_issues(
      "{setting_label}  start ({ttm_startdate}) is after (or equal to) {next_label} start({next_startdate})"
    )
}
