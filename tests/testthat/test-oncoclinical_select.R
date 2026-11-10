# oncoclinical_select() sin conexion real: se simulan la consulta de metadatos
# (DBI::dbGetQuery) y select_oncoclinical_tbl().

run_oncoclinical_select <- function(
  ...,
  .project = "p",
  .patients,
  metadata,
  tables
) {
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      oncoclinical_select(
        ...,
        .project = .project,
        .patients = .patients,
        .connection = NULL
      ),
      select_oncoclinical_tbl = function(tbl, ..., .connection) tables[[tbl]]
    ),
    dbGetQuery = function(...) metadata,
    .package = "DBI"
  )
}

metadata_one_table <- data.frame(
  TABLE_SCHEMA = "dbo",
  TABLE_NAME = "t1",
  COLUMN_NAME = c("ttm_startdate", "other_col"),
  DATA_TYPE = c("date", "varchar"),
  ORDINAL_POSITION = 1:2
)

tables_one <- list(
  t1 = tibble::tibble(
    dem_sap = c(1, 2, 3, 1),
    project = c("p", "p", "p", "other"),
    event_arm = 0,
    form = "t1",
    repeating_instance = 0,
    ttm_startdate = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")
  )
)

test_that("filters by project and patients", {
  out <- run_oncoclinical_select(
    "ttm_startdate",
    .patients = c(1, 3),
    metadata = metadata_one_table,
    tables = tables_one
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$dem_sap, c(1, 3))
  expect_true(all(out$project == "p"))
})

test_that("patients = NULL keeps every patient of the project", {
  out <- run_oncoclinical_select(
    "ttm_startdate",
    .patients = NULL,
    metadata = metadata_one_table,
    tables = tables_one
  )

  expect_equal(out$dem_sap, c(1, 2, 3))
  expect_true(all(out$project == "p"))
})

test_that("returns zero rows when patients is empty", {
  out <- run_oncoclinical_select(
    "ttm_startdate",
    .patients = numeric(0),
    metadata = metadata_one_table,
    tables = tables_one
  )

  expect_equal(nrow(out), 0)
})

test_that("converts date variables to Date", {
  out <- run_oncoclinical_select(
    "ttm_startdate",
    .patients = 1:3,
    metadata = metadata_one_table,
    tables = tables_one
  )

  expect_s3_class(out$ttm_startdate, "Date")
  expect_equal(out$ttm_startdate[1], as.Date("2020-01-01"))
})

test_that("returns one filtered tibble per table when variables span tables", {
  metadata <- data.frame(
    TABLE_SCHEMA = "dbo",
    TABLE_NAME = c("t1", "t2"),
    COLUMN_NAME = c("ttm_startdate", "pd_date"),
    DATA_TYPE = "date",
    ORDINAL_POSITION = 1L
  )
  tables <- list(
    t1 = tables_one$t1,
    t2 = tibble::tibble(
      dem_sap = c(1, 2),
      project = "p",
      event_arm = 0,
      form = "t2",
      repeating_instance = 0,
      pd_date = c("2021-01-01", "2021-02-01")
    )
  )

  out <- run_oncoclinical_select(
    "ttm_startdate",
    "pd_date",
    .patients = 2,
    metadata = metadata,
    tables = tables
  )

  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(out[[1]]$dem_sap, 2)
  expect_equal(out[[2]]$dem_sap, 2)
})
