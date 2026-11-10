# report_oncoclinical_verifications() escribe en un directorio temporal: se
# sustituye here::here() para no depender de la raiz del proyecto.

fake_verifications <- function(patients = NULL) {
  out <- tibble::tibble(
    verif_id = c("ocv_pl2", "ocv_pl3"),
    type = "plausibility",
    verification = c("v2", "v3"),
    details = NA_character_,
    n_issues = c(1L, 0L),
    issues = list(
      tibble::tibble(dem_sap = "1", issue = "x"),
      tibble::tibble(dem_sap = character(), issue = character())
    )
  )
  attr(out, "project") <- "breast"
  attr(out, "verification_datetime") <- as.POSIXct(
    "2026-11-06 10:30:00",
    tz = "UTC"
  )
  attr(out, "patients") <- patients
  out
}

run_report <- function(verifications) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- testthat::with_mocked_bindings(
    report_oncoclinical_verifications(verifications),
    here = function(...) file.path(dir, ...),
    .package = "here"
  )
  path
}

test_that("the report of all patients has no subset suffix", {
  path <- run_report(fake_verifications())

  expect_true(file.exists(path))
  expect_match(basename(path), "^breast_oncoclinical_verifications_2026")

  info <- openxlsx::read.xlsx(path, sheet = "project_info")
  expect_equal(info$patients, "All patients in the project")
})

test_that("the report of a subset is labelled as such", {
  path <- run_report(fake_verifications(patients = c(1, 2, 3)))

  expect_match(basename(path), "^breast_oncoclinical_verifications_subset_")

  info <- openxlsx::read.xlsx(path, sheet = "project_info")
  expect_equal(info$patients, "Subset of 3 patients")
})

test_that("only verifications with issues get their own sheet", {
  path <- run_report(fake_verifications())

  expect_equal(
    openxlsx::getSheetNames(path),
    c("project_info", "verifications_summary", "issues_by_sap", "ocv_pl2")
  )
})

test_that("issues_by_sap is the third sheet, right after the summary", {
  path <- run_report(fake_verifications())

  expect_equal(openxlsx::getSheetNames(path)[3], "issues_by_sap")

  issues <- openxlsx::read.xlsx(path, sheet = "issues_by_sap")
  expect_equal(issues$dem_sap, "1")
  expect_equal(issues$verif_id, "ocv_pl2")
})

test_that("issues_by_sap is omitted when there are no issues", {
  verifications <- fake_verifications()
  verifications$n_issues <- c(0L, 0L)
  path <- run_report(verifications)

  expect_equal(
    openxlsx::getSheetNames(path),
    c("project_info", "verifications_summary")
  )
})
