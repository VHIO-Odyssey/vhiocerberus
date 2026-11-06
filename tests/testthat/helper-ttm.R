# Helpers para simular la tabla `antineoplasic_therapy` sin conexion a la BBDD.

# Una fila cruda. Las fechas se pasan como texto ("YYYY-MM-DD"), igual que en
# la base de datos.
ttm_row <- function(
  setting = "Metastatic or Palliative",
  line = NA_character_,
  ad_int = NA_character_,
  start = NA_character_,
  end = NA_character_,
  pd = NA_character_,
  dem_sap = 1,
  event_arm = 0,
  stat = "Yes",
  project = "p"
) {
  tibble::tibble(
    dem_sap = dem_sap,
    project = project,
    event_arm = event_arm,
    ttm_stat = stat,
    ttm_setting = setting,
    ttm_met_line_num = line,
    ttm_ad_int = ad_int,
    ttm_startdate = start,
    ttm_enddate = end,
    ttm_pddate = pd
  )
}

# Fila de linea metastasica numerada
met_row <- function(line, start = NA_character_, ...) {
  ttm_row(
    setting = "Metastatic or Palliative",
    line = as.character(line),
    start = start,
    ...
  )
}

# Fila de adyuvancia tras cirugia de M1
m1_row <- function(start = NA_character_, ...) {
  ttm_row(
    setting = "Adjuvant",
    ad_int = "After M1 surgery",
    start = start,
    ...
  )
}

# Ejecuta extract_ordered_treatment() sobre las filas dadas. `repeating_instance`
# es la posicion de la fila en la entrada, para identificarlas en la salida.
# Si `patients` es NULL se piden todos los pacientes presentes en las filas.
run_extract <- function(..., project = "p", patients = NULL) {
  raw <-
    dplyr::bind_rows(...) |>
    dplyr::mutate(
      form = "antineoplasic_therapy",
      repeating_instance = dplyr::row_number(),
      .after = "event_arm"
    )

  if (is.null(patients)) {
    patients <- unique(raw$dem_sap)
  }

  testthat::with_mocked_bindings(
    extract_ordered_treatment(project, patients, connection = NULL),
    select_oncoclinical_tbl = function(tbl, ..., .connection) raw
  )
}
