test_that("NULL patients returns the data unchanged", {
  data <- tibble::tibble(dem_sap = 1:3)

  expect_identical(filter_patients(data, NULL), data)
})

test_that("keeps only the requested patients", {
  data <- tibble::tibble(dem_sap = c(1, 2, 2, 3), x = 1:4)

  expect_equal(filter_patients(data, c(2, 3))$x, 2:4)
  expect_equal(nrow(filter_patients(data, 99)), 0)
  expect_equal(nrow(filter_patients(data, numeric(0))), 0)
})

test_that("translates to SQL only when patients is not NULL", {
  skip_if_not_installed("dbplyr")

  lazy <- dbplyr::lazy_frame(
    dem_sap = 1,
    x = 1,
    con = dbplyr::simulate_mssql()
  )
  # Variable local con el mismo nombre que el argumento, como en el uso real
  patients <- c(2, 4)

  with_filter <- as.character(dbplyr::sql_render(filter_patients(
    lazy,
    patients
  )))
  without_filter <- as.character(dbplyr::sql_render(filter_patients(
    lazy,
    NULL
  )))

  expect_match(with_filter, "[dem_sap] IN (2.0, 4.0)", fixed = TRUE)
  expect_no_match(without_filter, "WHERE", fixed = TRUE)
})
