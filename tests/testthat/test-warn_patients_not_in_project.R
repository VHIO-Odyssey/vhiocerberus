demographics <- tibble::tibble(
  dem_sap = c(1, 2, 3, 4),
  project = c("p", "p", "other", "p")
)

run_warn <- function(patients, project = "p") {
  testthat::with_mocked_bindings(
    warn_patients_not_in_project(patients, project, connection = NULL),
    select_oncoclinical_tbl = function(tbl, ..., .connection) demographics
  )
}

test_that("does not query nor warn when patients is NULL", {
  testthat::with_mocked_bindings(
    expect_no_warning(
      warn_patients_not_in_project(NULL, "p", connection = NULL)
    ),
    select_oncoclinical_tbl = function(...) stop("should not be called")
  )
})

test_that("does not warn when every patient is in the project", {
  expect_no_warning(not_found <- run_warn(c(1, 2, 2)))
  expect_length(not_found, 0)
})

test_that("warns about patients that do not exist at all", {
  expect_warning(run_warn(c(1, 99)), "1 patient not found in project")
  expect_warning(run_warn(c(1, 98, 99)), "2 patients not found")
})

test_that("warns about patients that only belong to another project", {
  # dem_sap 3 existe, pero en "other"
  expect_warning(run_warn(c(1, 3)), "1 patient not found")

  not_found <- suppressWarnings(run_warn(c(1, 3)))
  expect_equal(not_found, 3)
})

test_that("the warning lists the missing patients", {
  expect_warning(run_warn(c(1, 99)), "99")
})

test_that("works with character identifiers", {
  expect_no_warning(run_warn(c("1", "2")))
  expect_warning(run_warn(c("1", "99")), "99")
})
