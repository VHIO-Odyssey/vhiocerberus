mk <- function(id, date, dem_sap = 1, event_arm = 0) {
  tibble::tibble(
    dem_sap = dem_sap,
    event_arm = event_arm,
    id = id,
    reference_date = as.Date(date)
  )
}

test_that("returns base untouched when there is nothing to insert", {
  base <- dplyr::bind_rows(mk("a", "2020-01-01"), mk("b", "2020-02-01"))

  expect_identical(insert_by_reference_date(base, base[0, ]), base)
})

test_that("places each row after the last base row with date <= its own", {
  base <- dplyr::bind_rows(
    mk("a", "2020-01-01"),
    mk("b", "2020-06-01"),
    mk("c", "2021-01-01")
  )
  to_insert <- dplyr::bind_rows(
    mk("x", "2020-03-01"),
    mk("y", "2022-01-01"),
    mk("z", "2019-01-01")
  )

  out <- insert_by_reference_date(base, to_insert)

  expect_equal(out$id, c("z", "a", "x", "b", "c", "y"))
})

test_that("ties go after the existing row with the same date", {
  base <- dplyr::bind_rows(mk("a", "2020-01-01"), mk("b", "2020-06-01"))

  out <- insert_by_reference_date(base, mk("x", "2020-06-01"))

  expect_equal(out$id, c("a", "b", "x"))
})

test_that("base order is preserved even when its dates are not monotone", {
  base <- dplyr::bind_rows(
    mk("a", "2020-06-01"),
    mk("b", "2020-01-01"),
    mk("c", "2020-09-01")
  )

  out <- insert_by_reference_date(base, mk("x", "2020-07-01"))

  # Tras "b", la ultima fila de base con fecha <= 2020-07-01
  expect_equal(out$id, c("a", "b", "x", "c"))
})

test_that("base rows with unknown date are never used as anchors", {
  base <- dplyr::bind_rows(
    mk("a", "2020-01-01"),
    mk("b", NA),
    mk("c", "2020-09-01")
  )

  out <- insert_by_reference_date(base, mk("x", "2020-05-01"))

  # "b" no ancla: x va tras "a", la ultima fila con fecha conocida <= la suya
  expect_equal(out$id, c("a", "x", "b", "c"))
})

test_that("inserted rows sharing a slot keep chronological order", {
  base <- dplyr::bind_rows(mk("a", "2020-01-01"), mk("b", "2021-01-01"))
  to_insert <- dplyr::bind_rows(
    mk("late", "2020-09-01"),
    mk("early", "2020-03-01"),
    mk("mid", "2020-06-01")
  )

  out <- insert_by_reference_date(base, to_insert)

  expect_equal(out$id, c("a", "early", "mid", "late", "b"))
})

test_that("inserted rows with unknown date go to the end of the patient block", {
  base <- dplyr::bind_rows(
    mk("a", "2020-01-01"),
    mk("b", "2020-06-01"),
    mk("c", "2020-01-01", dem_sap = 2)
  )

  out <- insert_by_reference_date(base, mk("x", NA))

  expect_equal(out$id, c("a", "b", "x", "c"))
})

test_that("rows are placed within their own patient and event_arm", {
  base <- dplyr::bind_rows(
    mk("a1", "2020-01-01", dem_sap = 1),
    mk("a2", "2020-06-01", dem_sap = 1),
    mk("b1", "2020-01-01", dem_sap = 1, event_arm = 1),
    mk("c1", "2020-01-01", dem_sap = 2)
  )
  to_insert <- dplyr::bind_rows(
    mk("x", "2020-03-01", dem_sap = 1, event_arm = 1),
    mk("y", "2020-03-01", dem_sap = 2)
  )

  out <- insert_by_reference_date(base, to_insert)

  expect_equal(out$id, c("a1", "a2", "b1", "x", "c1", "y"))
})

test_that("a patient absent from base is appended at the end", {
  base <- dplyr::bind_rows(mk("a", "2020-01-01"), mk("b", "2020-06-01"))
  to_insert <- dplyr::bind_rows(
    mk("x", "2020-03-01", dem_sap = 9),
    mk("y", "2020-05-01", dem_sap = 9)
  )

  out <- insert_by_reference_date(base, to_insert)

  expect_equal(out$id, c("a", "b", "x", "y"))
})

test_that("an empty base is handled", {
  base <- mk("a", "2020-01-01")[0, ]

  out <- insert_by_reference_date(
    base,
    dplyr::bind_rows(mk("y", "2020-02-01"), mk("x", "2020-01-01"))
  )

  expect_equal(out$id, c("x", "y"))
})

test_that("no helper columns are returned and extra columns are kept", {
  base <- mk("a", "2020-01-01") |> dplyr::mutate(extra = TRUE)
  to_insert <- mk("x", "2020-02-01") |> dplyr::mutate(extra = FALSE)

  out <- insert_by_reference_date(base, to_insert)

  expect_named(out, c("dem_sap", "event_arm", "id", "reference_date", "extra"))
  expect_equal(out$extra, c(TRUE, FALSE))
})
