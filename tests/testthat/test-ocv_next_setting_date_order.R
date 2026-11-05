test_that("repeating_instance of issues includes the next instance", {
  # Las filas llegan en orden distinto al cronologico: instance != order
  ordered_raw <- dplyr::bind_rows(
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

  out <- testthat::with_mocked_bindings(
    ocv_next_setting_date_order("p", connection = NULL, "ttm_startdate"),
    select_tbl = function(tbl, ..., .connection) {
      dplyr::mutate(
        ordered_raw,
        form = "antineoplasic_therapy",
        repeating_instance = dplyr::row_number(),
        .after = "event_arm"
      )
    }
  )

  expect_equal(nrow(out), 2)

  after <- out[out$dem_sap == 1, ]
  expect_equal(after$repeating_instance, "2, 1")
  expect_match(after$issue, "is after", fixed = TRUE)

  equal <- out[out$dem_sap == 2, ]
  expect_equal(equal$repeating_instance, "4, 3")
  expect_match(equal$issue, "is equal to", fixed = TRUE)

  expect_false(3 %in% out$dem_sap)
})
