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
        .data$repeating_instance,
        .data$next_instance,
        sep = ", "
      ),
      .ok = .data$ttm_enddate <= .data$next_ttm_startdate |
        is.na(.data$ttm_enddate) |
        is.na(.data$next_ttm_startdate)
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
        .data$repeating_instance,
        .data$next_instance,
        sep = ", "
      ),
      .ok = .data$ttm_pddate <= .data$next_ttm_startdate |
        is.na(.data$ttm_pddate) |
        is.na(.data$next_ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_pddate ({ttm_pddate}) is after {next_setting} ttm_startdate ({next_ttm_startdate})"
    )
}

ocv_pl9 <- function(project, patients, connection) {
  extract_ordered_treatment(project, patients, connection) |>
    dplyr::mutate(
      .ok = .data$ttm_pddate >= .data$ttm_startdate |
        is.na(.data$ttm_pddate) |
        is.na(.data$ttm_startdate)
    ) |>
    filter_issues(
      "{setting_label} ttm_pddate ({ttm_pddate}) is before ttm_startdate ({ttm_startdate})"
    )
}

ocv_pl10 <- function(project, patients, connection) {
  oncoclinical_select(
    oncoclinical_date_vars,
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    pivot_dates() |>
    dplyr::mutate(
      today = lubridate::today(),
      .ok = .data$date <= .data$today
    ) |>
    filter_issues(
      "{variable} ({date}) is after today ({today})"
    )
}

ocv_pl11 <- function(project, patients, connection) {
  dates <-
    oncoclinical_select(
      oncoclinical_date_vars[oncoclinical_date_vars != "dem_birthdate"],
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    pivot_dates()

  birth_date <-
    oncoclinical_select(
      "dem_birthdate",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(is.na(.data$dem_birthdate))

  dplyr::full_join(
    dates,
    birth_date,
    suffix = c("_1", "_2"),
    by = c("dem_sap", "event_arm")
  ) |>
    dplyr::mutate(
      .ok = .data$dem_birthdate < .data$date |
        is.na(.data$dem_birthdate) |
        is.na(.data$date)
    ) |>
    filter_issues(
      "{variable} ({date}) is before dem_birthdate ({dem_birthdate})"
    )
}

ocv_pl12 <- function(project, patients, connection) {
  dates <-
    oncoclinical_select(
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate",
      "pd_date",
      "surg_date",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    pivot_dates()

  diag_date <-
    oncoclinical_select(
      "tum_date_diagnosis",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(is.na(.data$tum_date_diagnosis))

  dplyr::inner_join(
    dates,
    diag_date,
    suffix = c("_1", "_2"),
    by = c("dem_sap", "event_arm")
  ) |>
    dplyr::mutate(
      .ok = tum_date_diagnosis <= date |
        is.na(tum_date_diagnosis) |
        is.na(date)
    ) |>
    filter_issues(
      "{variable} ({date}) is before tum_date_diagnosis ({tum_date_diagnosis})"
    )
}

ocv_pl13 <- function(project, patients, connection) {
  pd_crpc <-
    oncoclinical_select(
      "pd_crpc_date",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(is.na(.data$pd_crpc_date))

  first_ttm <-
    oncoclinical_select(
      "ttm_startdate",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter_out(is.na(.data$ttm_startdate)) |>
    dplyr::slice_min(
      .data$ttm_startdate,
      n = 1,
      with_ties = FALSE,
      by = c("dem_sap", "event_arm")
    )

  dplyr::inner_join(
    pd_crpc,
    first_ttm,
    by = c("dem_sap", "event_arm"),
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      .ok = .data$pd_crpc_date > .data$ttm_startdate
    ) |>
    filter_issues(
      "pd_crpc_date ({pd_crpc_date}) is before the first recorded ttm_startdate ({ttm_startdate})"
    )
}

ocv_pl14 <- function(project, patients, connection) {
  oncoclinical_select(
    "tum_psa",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = .data$tum_psa > 0 | is.na(.data$tum_psa)
    ) |>
    filter_issues(
      "tum_psa value is negative ({.data$tum_psa})"
    )
}

ocv_pl15 <- function(project, patients, connection) {
  biopsy_date <-
    oncoclinical_select(
      "metbiopsy_date",
      .project = project,
      .patients = patients,
      .connection = connection
    )

  diag_date <-
    oncoclinical_select(
      "tum_date_diagnosis",
      .project = project,
      .patients = patients,
      .connection = connection
    )

  dplyr::inner_join(
    biopsy_date,
    diag_date,
    by = c("dem_sap", "event_arm"),
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      .ok = .data$metbiopsy_date >= .data$tum_date_diagnosis |
        is.na(.data$metbiopsy_date) |
        is.na(.data$tum_date_diagnosis)
    ) |>
    filter_issues(
      "metbiopsy_date {metbiopsy_date} is before tum_date_diagnosis ({tum_date_diagnosis})"
    )
}

ocv_pl16 <- function(project, patients, connection) {
  oncoclinical_select(
    "tum_tumor_site",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = tum_tumor_site %in%
        c("Breast Right", "Breast Left") |
        is.na(.data$tum_tumor_site)
    ) |>
    filter_issues(
      "tum_tumor_site is {tum_tumor_site} (expected 'Breast Left', 'Breast Right', NA"
    )
}
