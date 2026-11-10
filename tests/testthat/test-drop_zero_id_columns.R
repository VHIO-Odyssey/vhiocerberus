test_that("drops event_arm and repeating_instance when all values are 0", {
  issues <- tibble::tibble(
    dem_sap = c("1", "2"),
    event_arm = c(0, 0),
    form = "f",
    repeating_instance = c(0, 0),
    issue = c("a", "b")
  )

  expect_named(
    drop_zero_id_columns(issues),
    c("dem_sap", "form", "issue")
  )
})

test_that("keeps the columns with any non-zero value", {
  issues <- tibble::tibble(
    dem_sap = c("1", "2"),
    event_arm = c(0, 1),
    repeating_instance = c("0, 1", "0"),
    issue = c("a", "b")
  )

  expect_named(drop_zero_id_columns(issues), names(issues))
})

test_that("handles suffixed columns independently", {
  issues <- tibble::tibble(
    dem_sap = "1",
    event_arm = 0,
    repeating_instance_1 = 0,
    repeating_instance_2 = 3,
    issue = "a"
  )

  expect_named(
    drop_zero_id_columns(issues),
    c("dem_sap", "repeating_instance_2", "issue")
  )
})

test_that("keeps columns with missing values instead of failing", {
  issues <- tibble::tibble(
    dem_sap = c("1", "2"),
    event_arm = c(0, NA),
    issue = c("a", "b")
  )

  expect_named(drop_zero_id_columns(issues), names(issues))
})

test_that("keeps every column when there are no issues", {
  issues <- tibble::tibble(
    dem_sap = character(),
    event_arm = numeric(),
    repeating_instance = numeric(),
    issue = character()
  )

  expect_named(drop_zero_id_columns(issues), names(issues))
})

test_that("does not touch an issue text that happens to be '0'", {
  issues <- tibble::tibble(dem_sap = "0", event_arm = 0, issue = "0")

  expect_named(drop_zero_id_columns(issues), c("dem_sap", "issue"))
})
