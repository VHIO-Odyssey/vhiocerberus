run_ocv <- function(fn, ..., patients = NULL) {
  raw <-
    dplyr::bind_rows(...) |>
    dplyr::mutate(
      form = "antineoplasic_therapy",
      repeating_instance = dplyr::row_number(),
      .after = "event_arm"
    )

  testthat::with_mocked_bindings(
    fn("p", patients, connection = NULL),
    select_oncoclinical_tbl = function(tbl, ..., .connection) raw
  )
}

test_that("ocv_pl7 flags ttm_enddate after the next ttm_startdate", {
  out <- run_ocv(
    ocv_pl7,
    met_row(1, "2020-01-01", end = "2020-03-01", dem_sap = 1),
    met_row(2, "2020-02-01", dem_sap = 1),
    met_row(1, "2020-01-01", end = "2020-02-01", dem_sap = 2),
    met_row(2, "2020-03-01", dem_sap = 2)
  )

  expect_equal(out$dem_sap, 1)
  expect_equal(out$repeating_instance, "1, 2")
  expect_match(out$issue, "ttm_enddate (2020-03-01) is after", fixed = TRUE)
})

test_that("ocv_pl8 flags ttm_pddate after the next ttm_startdate", {
  out <- run_ocv(
    ocv_pl8,
    met_row(1, "2020-01-01", pd = "2020-03-01", dem_sap = 1),
    met_row(2, "2020-02-01", dem_sap = 1),
    met_row(1, "2020-01-01", pd = "2020-01-15", dem_sap = 2),
    met_row(2, "2020-02-01", dem_sap = 2)
  )

  expect_equal(out$dem_sap, 1)
  expect_match(out$issue, "ttm_pddate (2020-03-01) is after", fixed = TRUE)
})

test_that("ocv_pl9 flags ttm_pddate before ttm_startdate", {
  out <- run_ocv(
    ocv_pl9,
    met_row(1, "2020-02-01", pd = "2020-01-01", dem_sap = 1),
    met_row(1, "2020-01-01", pd = "2020-02-01", dem_sap = 2),
    met_row(1, "2020-01-01", dem_sap = 3)
  )

  expect_equal(out$dem_sap, 1)
  expect_match(out$issue, "is before ttm_startdate", fixed = TRUE)
})

test_that("ocv_pl7 to ocv_pl9 respect the patients subset", {
  rows <- list(
    met_row(
      1,
      "2020-02-01",
      end = "2020-04-01",
      pd = "2020-01-01",
      dem_sap = 1
    ),
    met_row(2, "2020-03-01", dem_sap = 1),
    met_row(
      1,
      "2020-02-01",
      end = "2020-04-01",
      pd = "2020-01-01",
      dem_sap = 2
    ),
    met_row(2, "2020-03-01", dem_sap = 2)
  )

  for (fn in list(ocv_pl7, ocv_pl9)) {
    all_out <- do.call(run_ocv, c(list(fn), rows))
    subset_out <- do.call(run_ocv, c(list(fn), rows, list(patients = 2)))

    expect_equal(sort(unique(all_out$dem_sap)), c(1, 2))
    expect_equal(unique(subset_out$dem_sap), 2)
  }
})
