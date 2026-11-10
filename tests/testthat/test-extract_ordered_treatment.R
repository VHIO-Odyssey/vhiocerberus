test_that("returns the documented columns and no helper columns", {
  out <- run_extract(met_row(1, "2020-01-01"))

  expect_named(
    out,
    c(
      "dem_sap",
      "project",
      "event_arm",
      "form",
      "repeating_instance",
      "order",
      "setting_label",
      "reference_date",
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate"
    )
  )
  expect_s3_class(out, "tbl_df")
  expect_false(dplyr::is_grouped_df(out))
  expect_s3_class(out$reference_date, "Date")
  expect_s3_class(out$ttm_startdate, "Date")
})

test_that("keeps only the requested project and ttm_stat == 'Yes'", {
  out <- run_extract(
    met_row(1, "2020-01-01"),
    met_row(1, "2020-02-01", project = "other"),
    met_row(2, "2020-03-01", stat = "No"),
    met_row(2, "2020-04-01", stat = NA_character_)
  )

  expect_equal(out$repeating_instance, 1)
})

test_that("keeps only the requested patients", {
  rows <- list(
    met_row(1, "2020-01-01", dem_sap = 1), # 1
    met_row(1, "2020-02-01", dem_sap = 2), # 2
    met_row(1, "2020-03-01", dem_sap = 3) # 3
  )

  out <- do.call(run_extract, c(rows, list(patients = c(1, 3))))
  expect_equal(sort(out$dem_sap), c(1, 3))
  expect_equal(sort(out$repeating_instance), c(1, 3))

  # Un paciente que no esta en los datos no genera filas ni error
  out <- do.call(run_extract, c(rows, list(patients = c(2, 99))))
  expect_equal(out$dem_sap, 2)
})

test_that("patients = NULL applies no patient filter", {
  out <- run_extract(
    met_row(1, "2020-01-01", dem_sap = 1),
    met_row(1, "2020-02-01", dem_sap = 2),
    met_row(1, "2020-03-01", dem_sap = 3),
    patients = NULL
  )

  expect_equal(sort(out$dem_sap), 1:3)
})

test_that("returns zero rows when patients is empty", {
  out <- run_extract(met_row(1, "2020-01-01"), patients = numeric(0))

  expect_equal(nrow(out), 0)
  expect_true(all(
    c("order", "setting_label", "reference_date") %in% names(out)
  ))
})

test_that("returns zero rows when nothing matches", {
  out <- run_extract(met_row(1, "2020-01-01", stat = "No"))

  expect_equal(nrow(out), 0)
  expect_true(all(
    c("order", "setting_label", "reference_date") %in% names(out)
  ))
})

test_that("orders by setting, then line, then reference_date", {
  out <- run_extract(
    met_row(2, "2020-06-01"), # 1
    met_row(1, "2020-03-01"), # 2
    ttm_row("Adjuvant", start = "2019-01-01"), # 3
    ttm_row("Neoadjuvant", start = "2018-01-01"), # 4
    met_row(1, "2020-01-01") # 5
  )

  expect_equal(out$repeating_instance, c(4, 3, 5, 2, 1))
  expect_equal(
    out$setting_label,
    c("Neoadjuvant", "Adjuvant", "1L", "1L", "2L")
  )
  expect_equal(out$order, 1:5)
})

test_that("lines are ordered numerically, not alphabetically", {
  out <- run_extract(
    met_row(10, "2022-01-01"),
    met_row(2, "2021-01-01"),
    met_row(1, "2020-01-01")
  )

  expect_equal(out$setting_label, c("1L", "2L", "10L"))
})

test_that("order restarts per patient and event_arm and output is sorted", {
  out <- run_extract(
    met_row(1, "2020-01-01", dem_sap = 2, event_arm = 0),
    met_row(1, "2020-01-01", dem_sap = 1, event_arm = 1),
    met_row(2, "2020-02-01", dem_sap = 1, event_arm = 1),
    met_row(1, "2020-01-01", dem_sap = 1, event_arm = 0)
  )

  expect_equal(out$dem_sap, c(1, 1, 1, 2))
  expect_equal(out$event_arm, c(0, 1, 1, 0))
  expect_equal(out$order, c(1, 1, 2, 1))
})

test_that("reference_date falls back from start to end to pd", {
  out <- run_extract(
    met_row(1, start = "2020-01-01", pd = "2020-05-01"), # 1
    met_row(2, end = "2020-03-01", pd = "2020-05-01"), # 2
    met_row(3, pd = "2020-07-01"), # 3
    met_row(4) # 4
  )

  expect_equal(
    out$reference_date,
    as.Date(c("2020-01-01", "2020-03-01", "2020-07-01", NA))
  )
  # Una linea numerada sin ninguna fecha se mantiene, solo sin fecha
  expect_equal(out$repeating_instance, 1:4)
})

test_that("records that cannot be placed are dropped", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1 ok
    # Metastasico sin linea: problema de completeness
    ttm_row("Metastatic or Palliative", start = "2020-02-01"), # 2
    # Sin setting
    ttm_row(setting = NA_character_, start = "2020-03-01"), # 3
    # Setting raro pero sin fecha
    ttm_row("Maintenance"), # 4
    # Adyuvancia a M1 sin fecha
    m1_row() # 5
  )

  expect_equal(out$repeating_instance, 1)
})

test_that("adjuvant after M1 is inserted by reference_date", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    met_row(2, "2020-06-01"), # 2
    met_row(3, "2021-01-01"), # 3
    m1_row("2020-03-01") # 4: entre 1L y 2L
  )

  expect_equal(out$repeating_instance, c(1, 4, 2, 3))
  expect_equal(
    out$setting_label,
    c("1L", "Adjuvant Metastatic", "2L", "3L")
  )
  expect_equal(out$order, 1:4)
})

test_that("adjuvant after M1 can go first, last, or tie with an existing date", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    met_row(2, "2020-06-01"), # 2
    m1_row("2019-01-01"), # 3: antes de todo
    m1_row("2022-01-01"), # 4: despues de todo
    m1_row("2020-06-01") # 5: misma fecha que 2L, va despues
  )

  expect_equal(out$repeating_instance, c(3, 1, 2, 5, 4))
})

test_that("adjuvant after M1 uses fallback dates and is not re-ordered by NA dates", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    met_row(2, "2020-06-01"), # 2
    ttm_row("Adjuvant", ad_int = "After M1 surgery", end = "2020-03-01") # 3
  )

  expect_equal(out$repeating_instance, c(1, 3, 2))
})

test_that("a single run of adjuvant after M1 has no numeric suffix", {
  out <- run_extract(
    met_row(1, "2020-01-01"),
    m1_row("2020-02-01"),
    m1_row("2020-03-01"),
    met_row(2, "2020-06-01")
  )

  expect_equal(
    out$setting_label,
    c("1L", "Adjuvant Metastatic", "Adjuvant Metastatic", "2L")
  )
})

test_that("later runs of adjuvant after M1 are numbered, first one is not", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    m1_row("2020-02-01"), # 2: run 1
    met_row(2, "2020-06-01"), # 3
    m1_row("2020-08-01"), # 4: run 2
    m1_row("2020-09-01"), # 5: run 2
    met_row(3, "2021-01-01"), # 6
    m1_row("2021-06-01") # 7: run 3
  )

  expect_equal(out$repeating_instance, c(1, 2, 3, 4, 5, 6, 7))
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
})

test_that("run numbering is independent per patient and event_arm", {
  out <- run_extract(
    # dem_sap 1, arm 0: dos runs
    met_row(1, "2020-01-01"),
    m1_row("2020-02-01"),
    met_row(2, "2020-06-01"),
    m1_row("2020-08-01"),
    # dem_sap 1, arm 1: un run
    met_row(1, "2020-01-01", event_arm = 1),
    m1_row("2020-02-01", event_arm = 1),
    # dem_sap 2, arm 0: un run
    met_row(1, "2020-01-01", dem_sap = 2),
    m1_row("2020-08-01", dem_sap = 2)
  )

  adj <- out |> dplyr::filter(stringr::str_detect(setting_label, "^Adjuvant"))

  expect_equal(
    adj$setting_label,
    c(
      "Adjuvant Metastatic",
      "Adjuvant Metastatic 2",
      "Adjuvant Metastatic",
      "Adjuvant Metastatic"
    )
  )
})

test_that("ordinary settings are never suffixed, even when split by an insertion", {
  out <- run_extract(
    met_row(1, "2020-01-01"),
    met_row(1, "2020-03-01"),
    m1_row("2020-02-01")
  )

  expect_equal(
    out$setting_label,
    c("1L", "Adjuvant Metastatic", "1L")
  )
})

test_that("unorderable settings with label and date are inserted by date", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    met_row(2, "2020-06-01"), # 2
    ttm_row("Maintenance", start = "2020-03-01") # 3
  )

  expect_equal(out$repeating_instance, c(1, 3, 2))
  expect_equal(out$setting_label, c("1L", "Maintenance", "2L"))
  expect_equal(out$order, 1:3)
})

test_that("separate runs of an unorderable setting are numbered like M1 runs", {
  out <- run_extract(
    met_row(1, "2020-01-01"),
    ttm_row("Maintenance", start = "2020-03-01"),
    met_row(2, "2020-06-01"),
    ttm_row("Maintenance", start = "2020-08-01"),
    # Otro paciente con un unico run: sin sufijo
    met_row(1, "2020-01-01", dem_sap = 2),
    ttm_row("Maintenance", start = "2020-03-01", dem_sap = 2)
  )

  expect_equal(
    out$setting_label,
    c(
      "1L",
      "Maintenance",
      "2L",
      "Maintenance 2",
      "1L",
      "Maintenance"
    )
  )
})

test_that("M1 and unorderable records are interleaved chronologically", {
  out <- run_extract(
    met_row(1, "2020-01-01"), # 1
    met_row(2, "2020-12-01"), # 2
    m1_row("2020-05-01"), # 3
    ttm_row("Maintenance", start = "2020-03-01"), # 4
    m1_row("2020-07-01") # 5
  )

  expect_equal(out$repeating_instance, c(1, 4, 3, 5, 2))
  expect_equal(
    out$setting_label,
    c(
      "1L",
      "Maintenance",
      "Adjuvant Metastatic",
      "Adjuvant Metastatic",
      "2L"
    )
  )
})

test_that("inserted records do not alter the order of the ordered ones", {
  base <- run_extract(
    ttm_row("Neoadjuvant", start = "2018-01-01"),
    ttm_row("Adjuvant", start = "2019-01-01"),
    met_row(1, "2020-01-01"),
    met_row(2, "2020-06-01"),
    met_row(3, "2021-01-01")
  )

  with_inserted <- run_extract(
    ttm_row("Neoadjuvant", start = "2018-01-01"),
    ttm_row("Adjuvant", start = "2019-01-01"),
    met_row(1, "2020-01-01"),
    met_row(2, "2020-06-01"),
    met_row(3, "2021-01-01"),
    m1_row("2020-03-01"),
    m1_row("2019-06-01"),
    ttm_row("Maintenance", start = "2020-09-01")
  )

  kept <- with_inserted |> dplyr::filter(repeating_instance <= 5)
  expect_equal(kept$repeating_instance, base$repeating_instance)
  expect_equal(nrow(with_inserted), 8)
  expect_equal(with_inserted$order, 1:8)
})

test_that("a patient with only inserted records still gets an order", {
  out <- run_extract(
    met_row(1, "2020-01-01", dem_sap = 1),
    m1_row("2020-02-01", dem_sap = 2),
    m1_row("2020-03-01", dem_sap = 2)
  )

  only_m1 <- out |> dplyr::filter(dem_sap == 2)
  expect_equal(only_m1$order, 1:2)
  expect_equal(
    only_m1$setting_label,
    c("Adjuvant Metastatic", "Adjuvant Metastatic")
  )
})

test_that("records with NA ttm_ad_int are kept as ordinary records", {
  out <- run_extract(
    met_row(1, "2020-01-01", ad_int = NA_character_),
    met_row(2, "2020-06-01", ad_int = "After primary tumor surgery")
  )

  expect_equal(out$repeating_instance, 1:2)
})

test_that("works when there is nothing to insert", {
  out <- run_extract(
    met_row(2, "2020-06-01"),
    met_row(1, "2020-01-01")
  )

  expect_equal(out$repeating_instance, c(2, 1))
  expect_equal(out$order, 1:2)
  expect_false(".inserted" %in% names(out))
})
