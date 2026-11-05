# Regresion: add_next_setting_date() debe seguir funcionando con las etiquetas
# con sufijo ("Adjuvant Metastatic 2", "Maintenance 2", ...) que produce
# extract_ordered_treatment() para los runs posteriores de settings insertados.

test_that("next setting is resolved across suffixed labels", {
  ordered <- run_extract(
    met_row(1, "2020-01-01"), # 1L
    m1_row("2020-02-01"), # Adjuvant Metastatic
    met_row(2, "2020-06-01"), # 2L
    m1_row("2020-08-01"), # Adjuvant Metastatic 2
    m1_row("2020-09-01"), # Adjuvant Metastatic 2
    met_row(3, "2021-01-01"), # 3L
    m1_row("2021-06-01") # Adjuvant Metastatic 3
  )

  out <- add_next_setting_date(ordered, "reference_date")

  expect_equal(
    out$setting_label,
    c(
      "1L",
      "Adjuvant Metastatic",
      "2L",
      "Adjuvant Metastatic 2",
      "Adjuvant Metastatic 2",
      "3L",
      "Adjuvant Metastatic 3"
    )
  )
  expect_equal(
    out$next_setting,
    c(
      "Adjuvant Metastatic",
      "2L",
      "Adjuvant Metastatic 2",
      "3L",
      "3L",
      "Adjuvant Metastatic 3",
      NA
    )
  )
  expect_equal(out$next_instance, c(2, 3, 4, 6, 6, 7, NA))
  expect_equal(
    out$next_reference_date,
    as.Date(c(
      "2020-02-01",
      "2020-06-01",
      "2020-08-01",
      "2021-01-01",
      "2021-01-01",
      "2021-06-01",
      NA
    ))
  )
})

test_that("rows and columns of the input are preserved", {
  ordered <- run_extract(
    met_row(1, "2020-01-01"),
    m1_row("2020-02-01"),
    met_row(2, "2020-06-01"),
    m1_row("2020-08-01")
  )

  out <- add_next_setting_date(ordered, "reference_date")

  expect_equal(out[names(ordered)], ordered)
  expect_named(
    out,
    c(
      names(ordered),
      "next_reference_date",
      "next_instance",
      "next_setting"
    )
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("separate runs of an unorderable setting are told apart", {
  ordered <- run_extract(
    met_row(1, "2020-01-01"),
    ttm_row("Maintenance", start = "2020-03-01"),
    met_row(2, "2020-06-01"),
    ttm_row("Maintenance", start = "2020-08-01")
  )

  out <- add_next_setting_date(ordered, "reference_date")

  expect_equal(
    out$setting_label,
    c("1L", "Maintenance", "2L", "Maintenance 2")
  )
  expect_equal(
    out$next_setting,
    c("Maintenance", "2L", "Maintenance 2", NA)
  )
})

test_that("the next setting is never taken from another patient or event_arm", {
  ordered <- run_extract(
    met_row(1, "2020-01-01", dem_sap = 1),
    m1_row("2020-02-01", dem_sap = 1),
    met_row(1, "2020-01-01", dem_sap = 1, event_arm = 1),
    m1_row("2020-02-01", dem_sap = 2)
  )

  out <- add_next_setting_date(ordered, "reference_date")

  expect_equal(
    out$next_setting[out$dem_sap == 1 & out$event_arm == 0],
    c("Adjuvant Metastatic", NA)
  )
  expect_true(is.na(out$next_setting[out$dem_sap == 1 & out$event_arm == 1]))
  expect_true(is.na(out$next_setting[out$dem_sap == 2]))
})

test_that("records without date_var are skipped as next candidates", {
  ordered <- run_extract(
    met_row(1, "2020-01-01"),
    # Sin fecha de fin: no puede ser "siguiente" para date_var = ttm_enddate
    m1_row("2020-02-01"),
    met_row(2, "2020-06-01", end = "2020-07-01")
  ) |>
    dplyr::mutate(
      ttm_enddate = dplyr::if_else(
        setting_label == "1L",
        as.Date("2020-01-15"),
        ttm_enddate
      )
    )

  out <- add_next_setting_date(ordered, "ttm_enddate")

  expect_equal(out$next_setting[1], "2L")
  expect_equal(out$next_ttm_enddate[1], as.Date("2020-07-01"))
})
