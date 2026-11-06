# Filas con issues para los pacientes 1 y 2, y sin issues para el 3.
# Las filas llegan en orden distinto al cronologico: instance != order
ordered_raw <- function() {
  dplyr::bind_rows(
    # Paciente 1: 1L empieza despues que 2L -> "is after"
    met_row(2, "2020-03-01", dem_sap = 1), # instance 1
    met_row(1, "2020-06-01", dem_sap = 1), # instance 2
    # Paciente 2: 1L y 2L empiezan el mismo dia -> "is equal"
    met_row(2, "2020-01-01", dem_sap = 2), # instance 3
    met_row(1, "2020-01-01", dem_sap = 2), # instance 4
    # Paciente 3: orden correcto -> sin issues
    met_row(1, "2020-01-01", dem_sap = 3), # instance 5
    met_row(2, "2020-02-01", dem_sap = 3) # instance 6
  )
}

run_ocv_order <- function(patients, date_var = "ttm_startdate") {
  raw <-
    ordered_raw() |>
    dplyr::mutate(
      form = "antineoplasic_therapy",
      repeating_instance = dplyr::row_number(),
      .after = "event_arm"
    )

  testthat::with_mocked_bindings(
    ocv_next_setting_date_order("p", patients, connection = NULL, date_var),
    select_oncoclinical_tbl = function(tbl, ..., .connection) raw
  )
}

test_that("only the requested patients are verified", {
  expect_equal(sort(unique(run_ocv_order(1:3)$dem_sap)), c(1, 2))
  expect_equal(unique(run_ocv_order(2)$dem_sap), 2)
  expect_equal(nrow(run_ocv_order(3)), 0)
  expect_equal(nrow(run_ocv_order(numeric(0))), 0)
})

test_that("repeating_instance of issues includes the next instance", {
  out <- run_ocv_order(1:3)

  expect_equal(nrow(out), 2)

  after <- out[out$dem_sap == 1, ]
  expect_equal(after$repeating_instance, "2, 1")
  expect_match(after$issue, "is after", fixed = TRUE)

  equal <- out[out$dem_sap == 2, ]
  expect_equal(equal$repeating_instance, "4, 3")
  expect_match(equal$issue, "is equal to", fixed = TRUE)

  expect_false(3 %in% out$dem_sap)
})
