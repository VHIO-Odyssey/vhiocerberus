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

test_that("non-numeric identifiers are dropped without error", {
  data <- tibble::tibble(dem_sap = c(1, 2, 3), x = 1:3)

  expect_no_error(result <- filter_patients(data, c("2", "12A", "abc")))
  expect_equal(result$x, 2L)
  expect_equal(nrow(filter_patients(data, c("12A", "abc"))), 0)
})

test_that("non-numeric identifiers do not match missing dem_sap", {
  data <- tibble::tibble(dem_sap = c(1, NA, 3), x = 1:3)

  expect_equal(filter_patients(data, c("1", "12A"))$x, 1L)
  expect_equal(nrow(filter_patients(data, NA)), 0)
})

test_that("factor identifiers are matched by label, not by code", {
  data <- tibble::tibble(dem_sap = c(10, 20, 30), x = 1:3)

  expect_equal(filter_patients(data, factor(c("30", "10")))$x, c(1L, 3L))
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
