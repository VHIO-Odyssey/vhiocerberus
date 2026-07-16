verify_oncoclinical_data <- function(user, password, tumor) {
  connection <- connect_to_vhio_database(user, password, "OncoClinical")
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  population <-
    dplyr::tbl(connection, "demographics") |>
    dplyr::filter(.data$dem_tumor_type == tumor) |>
    dplyr::select("dem_sap") |>
    dplyr::collect() |>
    dplyr::pull("dem_sap")

  current_verifications <-
    oncoclinical_verifications |>
    dplyr::filter(.data[[tumor]] == 1) |>
    dplyr::pull("verif_id")

  purrr::map(
    current_verifications,
    ~ do.call(., list(population = population, connection = connection))
  ) |>
    purrr::set_names(current_verifications)
}


# Verificaciones de la base de datos OncoClinical.
# No se exportan, las funciones se llaman internamente desde la función
# `verify_oncoclinical_data()`.
# Las verificaciones requieren una conexión a OncoClinical y una población de
# pacientes sobre los que aplicar las verificaciones.

oncoclinical_1 <- function(population, connection) {
  # Verificar que pd_date es anterior a linea 1 y si no indicar entre que fechas
  # se encuentra

  print("Running oncoclinical_1 verification")

  first_pd <-
    dplyr::tbl(connection, "first_progression") |>
    dplyr::filter(.data$dem_sap %in% population) |>
    dplyr::select("dem_sap", "event_arm", "pd_date") |>
    dplyr::collect() |>
    dplyr::mutate(pd_date = lubridate::ymd(.data$pd_date))

  ttms <-
    dplyr::tbl(connection, "antineoplasic_therapy") |>
    dplyr::select(
      "dem_sap",
      "event_arm",
      "ttm_met_line_num",
      "ttm_startdate",
      "ttm_enddate"
    ) |>
    dplyr::filter(
      .data$dem_sap %in% population,
      !is.na(.data$ttm_met_line_num)
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      ttm_startdate = lubridate::ymd(.data$ttm_startdate),
      ttm_enddate = lubridate::ymd(.data$ttm_enddate)
    ) |>
    dplyr::summarise(
      ttm_startdate = safe_min(.data$ttm_startdate),
      ttm_enddate = safe_max(.data$ttm_enddate),
      .by = c("dem_sap", "event_arm", "ttm_met_line_num")
    )

  ttms_wide <-
    dplyr::inner_join(first_pd, ttms, by = c("dem_sap", "event_arm")) |>
    tidyr::pivot_wider(
      names_from = "ttm_met_line_num",
      values_from = c("ttm_startdate", "ttm_enddate"),
      names_sep = "___"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ok = all(
        .data$pd_date <= dplyr::c_across(dplyr::matches("___\\d+$")),
        na.rm = TRUE
      ),
    ) |>
    dplyr::ungroup() |>
    dplyr::filter_out(.data$ok)

  ttms_wide_list <-
    purrr::map(
      1:nrow(ttms_wide),
      ~ ttms_wide[., ]
    )

  purrr::map(
    ttms_wide_list,
    \(x) {
      case_dates_row <- dplyr::select(x, dplyr::matches("___\\d+$"))
      case_dates_vct <-
        vctrs::vec_c(!!!case_dates_row) |>
        na.omit()
      case_dates_ordered <- case_dates_vct[order(case_dates_vct)]

      next_dates_index <- case_dates_ordered >= x$pd_date
      prev_dates_index <- case_dates_ordered < x$pd_date

      next_date <- case_dates_ordered[next_dates_index][1]
      prev_date <- case_dates_ordered[prev_dates_index] |>
        tail(1)

      next_date_label <-
        stringr::str_c(
          stringr::str_extract(names(next_date), "\\d+$"),
          "L ",
          stringr::str_extract(names(next_date), "start|end") |>
            stringr::str_to_title()
        )
      prev_date_label <-
        stringr::str_c(
          stringr::str_extract(names(prev_date), "\\d+$"),
          "L ",
          stringr::str_extract(names(prev_date), "start|end") |>
            stringr::str_to_title()
        )

      dplyr::tibble(
        dem_sap = x$dem_sap,
        event = x$event_arm,
        prev_date_type = prev_date_label,
        prev_date = prev_date,
        pd_date = x$pd_date,
        next_date_type = next_date_label,
        next_date = next_date
      )
    }
  ) |>
    purrr::list_rbind()
}
