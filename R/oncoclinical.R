verify_oncoclinical_data <- function(user, password, project) {
  connection <- connect_to_vhio_database(user, password, "OncoClinical")
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  current_verifications <-
    oncoclinical_verifications |>
    dplyr::filter(.data[[project]] == 1) |>
    dplyr::pull("verif_id")

  purrr::map(
    current_verifications,
    ~ do.call(., list(project = project, connection = connection))
  ) |>
    purrr::set_names(current_verifications)
}
