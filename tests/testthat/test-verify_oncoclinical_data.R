# verify_oncoclinical_data() sin conexion real: se simulan la conexion, la
# tabla de demografia y las verificaciones ocv_*.

verif_ids <- paste0("ocv_pl", 1:9)

# Ejecuta verify_oncoclinical_data() con las verificaciones sustituidas por
# `stub`. Devuelve el resultado y los `patients` que recibio cada verificacion.
run_verify <- function(project = "breast", patients = NULL, stub = NULL) {
  seen <- new.env()
  seen$patients <- list()

  if (is.null(stub)) {
    stub <- function(project, patients, connection) {
      tibble::tibble(dem_sap = 1, event_arm = 0, issue = "x")
    }
  }

  recording_stub <- function(project, patients, connection) {
    seen$patients <- c(seen$patients, list(patients))
    stub(project, patients, connection)
  }
  stubs <- stats::setNames(
    rep(list(recording_stub), length(verif_ids)),
    verif_ids
  )

  demographics <- tibble::tibble(
    dem_sap = 1:4,
    project = c("breast", "breast", "crc", "breast")
  )

  out <- suppressMessages(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        testthat::with_mocked_bindings(
          verify_oncoclinical_data(
            "user",
            "password",
            project = project,
            patients = patients
          ),
          !!!stubs
        ),
        connect_to_vhio_database = function(...) "connection",
        select_oncoclinical_tbl = function(tbl, ..., .connection) demographics
      ),
      dbDisconnect = function(...) invisible(TRUE),
      .package = "DBI"
    )
  )

  list(out = out, seen = seen$patients)
}

test_that("defaults to every patient of the project", {
  res <- run_verify("breast")

  expect_true(length(res$seen) > 0)
  for (p in res$seen) {
    expect_equal(sort(p), c(1, 2, 4))
  }
})

test_that("passes an explicit subset of patients to every verification", {
  res <- run_verify("breast", patients = c(2, 4))

  expect_true(length(res$seen) > 0)
  for (p in res$seen) {
    expect_equal(p, c(2, 4))
  }
})

test_that("runs only the verifications enabled for the project", {
  res <- run_verify("breast")

  enabled <-
    oncoclinical_verifications$verif_id[oncoclinical_verifications$breast == 1]
  expect_equal(res$out$verif_id, enabled)
  expect_equal(length(res$seen), length(enabled))
})

test_that("returns issues, counts and attributes", {
  res <- run_verify("breast", patients = 1)

  expect_equal(unique(res$out$n_issues), 1L)
  expect_true(all(purrr::map_lgl(res$out$issues, \(x) x$dem_sap == "1")))
  expect_equal(attr(res$out, "project"), "breast")
  expect_s3_class(attr(res$out, "verification_datetime"), "POSIXct")
})

test_that("a failing verification gives NULL issues and NA n_issues", {
  res <- run_verify(
    "breast",
    stub = function(project, patients, connection) stop("boom")
  )

  expect_true(all(is.na(res$out$n_issues)))
  expect_true(all(purrr::map_lgl(res$out$issues, is.null)))
})
