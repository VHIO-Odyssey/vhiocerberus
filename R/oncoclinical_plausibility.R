# OncoClinical plausibility verifications
# named as ocv_pl<number>.

ocv_pl1 <- function(project, patients, connection) {
  # pd_date is before any advanced disease treatment
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
      "{ttm_met_line_num}L ttm_startdate ({ttm_startdate}) is before pd_date ({pd_date})"
    )
}

ocv_pl2 <- function(project, patients, connection) {
  # ttm_startdate is before or equal ttm_enddate
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
  # ttm_startdate and ttm_enddate are different across instances
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
  # ttm_startdate is before the ttm_startdate of the next setting
  ocv_next_setting_date_order(project, patients, connection, "ttm_startdate")
}

ocv_pl5 <- function(project, patients, connection) {
  # ttm_enddate is before the ttm_enddate of the next setting
  ocv_next_setting_date_order(project, patients, connection, "ttm_enddate")
}

ocv_pl6 <- function(project, patients, connection) {
  # ttm_pddate is before the ttm_pddate of the next setting
  ocv_next_setting_date_order(project, patients, connection, "ttm_pddate")
}

ocv_pl7 <- function(project, patients, connection) {
  # ttm_enddate is before or equal to the ttm_startdate of the next setting
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
  # ttm_pddate is before or equal to the ttm_startdate of the next setting
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
  # ttm_pddate is after or equal to ttm_startdate
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
  # all date variables are before today.
  oncoclinical_select(
    get_oncoclinical_date_variables(connection),
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
  # all date variables are after dem_birthdate
  date_vars <- get_oncoclinical_date_variables(connection)

  dates <-
    oncoclinical_select(
      date_vars[date_vars != "dem_birthdate"],
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
  # ttm_startdate, ttm_enddate, ttm_pddate, pd_date, surg_date are after tum_date_diagnosis
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
      .ok = .data$tum_date_diagnosis <= .data$date |
        is.na(.data$tum_date_diagnosis) |
        is.na(.data$date)
    ) |>
    filter_issues(
      "{variable} ({date}) is before tum_date_diagnosis ({tum_date_diagnosis})"
    )
}

ocv_pl13 <- function(project, patients, connection) {
  # pd_crpc_date is after first ttm_startdate
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
  # tum_psa value is positive
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
  # metbiopsy_date is after or equal to tum_date_diagnosis
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
  # tum_tumor_site is 'breast_left' or 'breast_right' or NA
  oncoclinical_select(
    "tum_tumor_site",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = .data$tum_tumor_site %in%
        c("Breast Right", "Breast Left") |
        is.na(.data$tum_tumor_site)
    ) |>
    filter_issues(
      "tum_tumor_site is {tum_tumor_site} (expected 'Breast Left', 'Breast Right', NA"
    )
}

ocv_pl17 <- function(project, patients, connection) {
  # (tum_m_at_diagnosis = 'M1' and tum_tnm_stage = 'IV' and tum_tnm_group = 'IV') or (tum_m_at_diagnosis != 'M1' and tum_tnm_stage != 'IV' and tum_tnm_group != 'IV')
  oncoclinical_select(
    "tum_m_at_diagnosis",
    "tum_tnm_stage",
    "tum_tnm_group",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    # sólo centrado en los casos con las tres variables completas.
    # Problemas por falta de dato son un issue de completeness
    na.omit() |>
    dplyr::mutate(
      advanced_stage = .data$tum_m_at_diagnosis == "M1" &
        .data$tum_tnm_stage == "IV" &
        .data$tum_tnm_group == "IV",
      early_stage = .data$tum_m_at_diagnosis != "M1" &
        .data$tum_tnm_stage != "IV" &
        .data$tum_tnm_group != "IV",
      .ok = .data$advanced_stage | .data$early_stage
    ) |>
    filter_issues(
      "Unaccepted combination of tum_m_at_diagnosis ({tum_m_at_diagnosis}), tum_tnm_stage ({tum_tnm_stage}) and tum_tnm_group ({tum_tnm_group})"
    )
}

ocv_pl18 <- function(project, patients, connection) {
  # (tum_stage_dx = 'Metastatic' and  tum_m_at_diagnosis = 'M1' and tum_tnm_stage = 'IV') or (tum_stage_dx != 'Metastatic' and  tum_m_at_diagnosis != 'M1' and tum_tnm_stage != 'IV')
  oncoclinical_select(
    "tum_stage_dx",
    "tum_m_at_diagnosis",
    "tum_tnm_stage",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    # sólo centrado en los casos con las tres variables completas.
    # Problemas por falta de dato son un issue de completeness
    na.omit() |>
    dplyr::mutate(
      advanced_stage = .data$tum_stage_dx == "Metastatic" &
        .data$tum_m_at_diagnosis == "M1" &
        .data$tum_tnm_stage == "IV",
      early_stage = .data$tum_stage_dx != "Metastatic" &
        .data$tum_m_at_diagnosis != "M1" &
        .data$tum_tnm_stage != "IV",
      .ok = .data$advanced_stage | .data$early_stage
    ) |>
    filter_issues(
      "Unaccepted combination of tum_stage_dx ({tum_stage_dx}), tum_m_at_diagnosis ({tum_m_at_diagnosis}) and tum_tnm_stage ({tum_tnm_stage})"
    )
}

ocv_pl19 <- function(project, patients, connection) {
  # (tum_m_at_diagnosis = 'M1' and tum_tnm_group = 'IVB') or (tum_m_at_diagnosis != 'M1' and tum_tnm_group != 'IVB')
  oncoclinical_select(
    "tum_m_at_diagnosis",
    "tum_tnm_group",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    na.omit() |>
    dplyr::mutate(
      .ok = (.data$tum_m_at_diagnosis == "M1" & .data$tum_tnm_group == "IVB") |
        (.data$tum_m_at_diagnosis != "M1" & .data$tum_tnm_group != "IVB")
    ) |>
    filter_issues(
      "Unexpected combination of tum_m_at_diagnosis ({tum_m_at_diagnosis}) and tum_tnm_group ({tum_tnm_group}) "
    )
}

ocv_pl20 <- function(project, patients, connection) {
  # (tum_sync_met = 'Yes' and tum_m_at_diagnosis = 'M1' and tum_tnm_stage= 'IV') or (tum_sync_met != 'Yes' and tum_m_at_diagnosis != 'M1' and tum_tnm_stage != 'IV')
  oncoclinical_select(
    "tum_sync_met",
    "tum_m_at_diagnosis",
    "tum_tnm_stage",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    # sólo centrado en los casos con las tres variables completas.
    # Problemas por falta de dato son un issue de completeness
    na.omit() |>
    dplyr::mutate(
      advanced_stage = .data$tum_sync_met == "Yes" &
        .data$tum_m_at_diagnosis == "M1" &
        .data$tum_tnm_stage == "IV",
      early_stage = .data$tum_sync_met == "No" &
        .data$tum_m_at_diagnosis != "M1" &
        .data$tum_tnm_stage != "IV",
      .ok = .data$advanced_stage | .data$early_stage
    ) |>
    filter_issues(
      "Unaccepted combination of tum_sync_met ({tum_sync_met}), tum_m_at_diagnosis ({tum_m_at_diagnosis}) and tum_tnm_stage ({tum_tnm_stage})"
    )
}

ocv_pl21 <- function(project, patients, connection) {
  # pd_date is before or equal to pd_distant_date
  oncoclinical_select(
    "pd_date",
    "pd_distant_date",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = .data$pd_date <= .data$pd_distant_date |
        is.na(.data$pd_date) |
        is.na(.data$pd_distant_date)
    ) |>
    filter_issues(
      "pd_date ({pd_date}) is after pd_distant_date ({pd_distant_date})"
    )
}

ocv_pl22 <- function(project, patients, connection) {
  # surv_last_fu_date or surv_death_date are equal or after all the variable dates (from instruments !Survival)
  #wip valorar verificación solo cognos
  # en esta verificación salen muchas fechas de cognos. Valorar con data
  # curators si crear una verificación tipo completeness. La idea es que si van
  # entrando fechas de cognos más allá del fup, eso significa que el caso está
  # desactualizado.
  date_vars <- get_oncoclinical_date_variables(connection) |>
    stringr::str_subset("^surv_", negate = TRUE)

  dates <-
    oncoclinical_select(
      date_vars,
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    pivot_dates()

  surv_dates <-
    oncoclinical_select(
      "surv_last_fu_date",
      "surv_death_date",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::mutate(
      surv_ref_date = dplyr::if_else(
        !is.na(.data$surv_death_date),
        .data$surv_death_date,
        .data$surv_last_fu_date
      ),
      surv_ref_date_name = dplyr::if_else(
        !is.na(.data$surv_death_date),
        "surv_death_date",
        "surv_last_fu_date"
      )
    ) |>
    dplyr::select(-"surv_last_fu_date", -"surv_death_date")

  dplyr::inner_join(
    dates,
    surv_dates,
    by = c("dem_sap", "event_arm"),
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      .ok = .data$date <= .data$surv_ref_date |
        is.na(.data$date) |
        is.na(.data$surv_ref_date)
    ) |>
    filter_issues(
      "{variable} ({date}) is after {surv_ref_date_name} ({surv_ref_date})"
    )
}

ocv_pl23 <- function(project, patients, connection) {
  # surv_date_reviewed is equal or after surv_last_fu_date or surv_death_date
  oncoclinical_select(
    "surv_last_fu_date",
    "surv_death_date",
    "surv_date_reviewed",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      surv_ref_date = dplyr::if_else(
        !is.na(.data$surv_death_date),
        .data$surv_death_date,
        .data$surv_last_fu_date
      ),
      surv_ref_date_name = dplyr::if_else(
        !is.na(.data$surv_death_date),
        "surv_death_date",
        "surv_last_fu_date"
      )
    ) |>
    dplyr::mutate(
      .ok = .data$surv_date_reviewed >= .data$surv_ref_date |
        is.na(.data$surv_date_reviewed) |
        is.na(.data$surv_ref_date)
    ) |>
    filter_issues(
      "{surv_ref_date_name} ({surv_ref_date}) is after surv_date_reviewed ({surv_date_reviewed})"
    )
}

ocv_pl24 <- function(project, patients, connection) {
  # (mol_claudine18_val >= 75 and mol_claudine18_cutoff = '>= 75') or (mol_claudine18_val < 75 AND mol_claudine18_cutoff =  '<75')
  oncoclinical_select(
    "mol_claudine18_val",
    "mol_claudine18_cutoff",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = (.data$mol_claudine18_val >= 75 &
        .data$mol_claudine18_cutoff == ">= 75") |
        (.data$mol_claudine18_val < 75 &
          .data$mol_claudine18_cutoff == "<75") |
        is.na(.data$mol_claudine18_val) |
        is.na(.data$mol_claudine18_cutoff)
    ) |>
    filter_issues(
      "mol_claudine18_val ({mol_claudine18_val}) and mol_claudine18_cutoff ({mol_claudine18_cutoff}) do not match"
    )
}

ocv_pl25 <- function(project, patients, connection) {
  # pd_stat = 'No' and all ttm_setting != 'Metastatic or Palliative'
  pd_stat_no <-
    oncoclinical_select(
      "pd_stat",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter(.data$pd_stat == "No")

  met_settings <-
    oncoclinical_select(
      "ttm_setting",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter(.data$ttm_setting == "Metastatic or Palliative") |>
    dplyr::summarise(
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        collapse = ", "
      ),
      .by = c("dem_sap", "event_arm", "form")
    )

  dplyr::left_join(
    pd_stat_no,
    met_settings,
    by = c("dem_sap", "event_arm"),
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      .ok = is.na(.data$form_2)
    ) |>
    filter_issues(
      "pd_stat = 'No' but the patients have 'Metastatic or Palliative' setting(s)"
    )
}

ocv_pl26 <- function(project, patients, connection) {
  # pd_stat = 'No' and tum_m_at_diagnosis != 'M1' and all ttm_setting != 'Metastatic or Palliative'
  pd_stat_no <-
    oncoclinical_select(
      "pd_stat",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter(.data$pd_stat == "No")

  no_m1 <-
    oncoclinical_select(
      "tum_m_at_diagnosis",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter(.data$tum_m_at_diagnosis != "M1")

  pd_m1_no <-
    dplyr::inner_join(
      pd_stat_no,
      no_m1,
      by = c("dem_sap", "event_arm"),
      suffix = c("_1", "_2")
    )

  met_settings <-
    oncoclinical_select(
      "ttm_setting",
      .project = project,
      .patients = patients,
      .connection = connection
    ) |>
    dplyr::filter(.data$ttm_setting == "Metastatic or Palliative") |>
    dplyr::summarise(
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        collapse = ", "
      ),
      .by = c("dem_sap", "event_arm", "form")
    ) |>
    dplyr::rename(
      form_3 = "form",
      repeating_instance_3 = "repeating_instance"
    )

  dplyr::left_join(
    pd_m1_no,
    met_settings,
    by = c("dem_sap", "event_arm")
  ) |>
    dplyr::mutate(
      .ok = is.na(.data$form_3)
    ) |>
    filter_issues(
      "pd_stat = 'No' and tum_m_at_diagnosis != 'M1' but the patients have 'Metastatic or Palliative' setting(s)"
    )
}

ocv_pl27 <- function(project, patients, connection) {
  # surv_death_date equal or after surv_last_fu_date
  oncoclinical_select(
    "surv_last_fu_date",
    "surv_death_date",
    .project = project,
    .patients = patients,
    .connection = connection
  ) |>
    dplyr::mutate(
      .ok = .data$surv_death_date >= .data$surv_last_fu_date |
        is.na(.data$surv_last_fu_date) |
        is.na(.data$surv_death_date)
    ) |>
    filter_issues(
      "surv_death_date ({surv_death_date}) is before surv_last_fu_date ({surv_last_fu_date})"
    )
}
