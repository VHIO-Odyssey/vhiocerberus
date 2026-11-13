# OncoClinical completeness verifications
# named as ocv_cm<number>.

ocv_cm1 <- function(project, patients, connection, oncopharma) {
  last_ttm_enddate <-
    extract_ordered_treatment(project, patients, connection) |>
    dplyr::slice_max(.data$order, n = 1, by = c("dem_sap", "event_arm"))

  or_ttm <-
    dplyr::tbl(oncopharma, "Treatments_Oral") |>
    dplyr::select("sap", "ttm_date") |>
    dplyr::filter(.data$sap %in% last_ttm_enddate$dem_sap) |>
    dplyr::collect()

  iv_ttm <-
    dplyr::tbl(oncopharma, "Treatments_Intravenous") |>
    dplyr::select("sap", "ttm_administration_date") |>
    dplyr::filter(.data$sap %in% last_ttm_enddate$dem_sap) |>
    dplyr::collect()

  last_op_ttm <-
    dplyr::bind_rows(
      dplyr::rename(or_ttm, ttm_date = "ttm_date"),
      dplyr::rename(iv_ttm, ttm_date = "ttm_administration_date")
    ) |>
    dplyr::mutate(
      ttm_date = lubridate::ymd(.data$ttm_date)
    ) |>
    dplyr::summarise(
      last_op_date = max(.data$ttm_date, na.rm = TRUE),
      .by = "sap"
    ) |>
    dplyr::rename(dem_sap = "sap")

  dplyr::inner_join(last_ttm_enddate, last_op_ttm, by = "dem_sap") |>
    dplyr::mutate(
      .ok = .data$ttm_enddate >= .data$last_op_date |
        is.na(.data$last_op_date) |
        is.na(.data$ttm_enddate)
    ) |>
    filter_issues(
      "Last treatment ttm_enddate  ({setting_label}: {ttm_enddate}) is earlier than the last administration in OncoPharma ({last_op_date})"
    )
}
