# Verificaciones para la base de datos OncoClinical
oncoclinical_verifications <- readr::read_csv2(
  here::here("data-raw", "oncoclinical_verifications.csv")
)

usethis::use_data(oncoclinical_verifications, overwrite = TRUE)
