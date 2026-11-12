# get_oncoclinical_date_variables() sin conexion real: se simula DBI::dbGetQuery()
# segun la consulta (nombre de la base de datos o metadatos de columnas).

run_get_date_vars <- function(db = "OncoClinical", date_columns = character()) {
  queries <- character()

  out <- testthat::with_mocked_bindings(
    tryCatch(
      get_oncoclinical_date_variables(connection = NULL),
      error = function(e) e
    ),
    dbGetQuery = function(conn, statement, ...) {
      queries <<- c(queries, statement)
      if (grepl("DB_NAME()", statement, fixed = TRUE)) {
        data.frame(db = db)
      } else {
        data.frame(COLUMN_NAME = date_columns)
      }
    },
    .package = "DBI"
  )

  list(out = out, queries = queries)
}

test_that("returns the date column names as a character vector", {
  res <- run_get_date_vars(date_columns = c("dem_birthdate", "ttm_startdate"))

  expect_type(res$out, "character")
  expect_equal(res$out, c("dem_birthdate", "ttm_startdate"))
})

test_that("returns a zero-length vector when there are no date columns", {
  res <- run_get_date_vars(date_columns = character())

  expect_type(res$out, "character")
  expect_length(res$out, 0)
})

test_that("checks the database before querying the columns", {
  res <- run_get_date_vars(date_columns = "ttm_startdate")

  expect_length(res$queries, 2)
  expect_match(res$queries[[1]], "DB_NAME()", fixed = TRUE)
  expect_match(res$queries[[2]], "INFORMATION_SCHEMA.COLUMNS", fixed = TRUE)
})

test_that("the column query targets only the SQL Server date type", {
  res <- run_get_date_vars(date_columns = "ttm_startdate")

  expect_match(res$queries[[2]], "DATA_TYPE = 'date'", fixed = TRUE)
  expect_no_match(res$queries[[2]], "datetime", fixed = TRUE)
})

test_that("errors when connected to a database other than OncoClinical", {
  res <- run_get_date_vars(db = "OncoPharma", date_columns = "ttm_startdate")

  expect_s3_class(res$out, "error")
  expect_match(conditionMessage(res$out), "OncoClinical")
  expect_match(conditionMessage(res$out), "OncoPharma")
})

test_that("does not query the columns when the database is wrong", {
  res <- run_get_date_vars(db = "OncoPharma", date_columns = "ttm_startdate")

  expect_length(res$queries, 1)
  expect_match(res$queries[[1]], "DB_NAME()", fixed = TRUE)
})

test_that("the database name check is case sensitive and exact", {
  expect_s3_class(run_get_date_vars(db = "oncoclinical")$out, "error")
  expect_s3_class(run_get_date_vars(db = "OncoClinical_test")$out, "error")
})
