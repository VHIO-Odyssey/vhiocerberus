# Function to connect to the VHIO database using ODBC
# usert: Username for the database connection
# password: Password for the database connection
# database_name: Name of the database to connect to (e.g., "OncoClinical",
# "OncoPharma", "Prescreening_NEW")
connect_to_vhio_database <- function(
  user,
  password,
  database_name
) {
  vhio_reach <- tryCatch(
    {
      # Try to establish a socket connection to the Server
      con <- socketConnection(
        host = "172.27.254.6",
        port = 1433,
        blocking = TRUE,
        timeout = 1
      )
      # If no errors occur, the network is accessible. Close the test
      # connection.
      close(con)
      TRUE
    },
    error = function(e) {
      # If an error occurs (like a timeout), return FALSE
      FALSE
    },
    warning = function(w) {
      FALSE
    }
  )

  if (!vhio_reach) {
    cli::cli_abort(c(
      "x" = "Cannot reach the VHIO Data Lake.",
      "i" = "Make sure you are connected to the VHIO network or VPN."
    ))
  }

  connection <- tryCatch(
    {
      odbc::dbConnect(
        odbc::odbc(),
        Driver = "SQL Server",
        Server = "172.27.254.6",
        Database = database_name,
        UID = user,
        PWD = password,
        Port = 1433
      )
    },
    error = function(e) {
      FALSE
    }
  )

  if (isFALSE(connection)) {
    cli::cli_abort(c(
      "x" = "Failed to establish a connection to {.val {database_name}}.",
      "i" = "Please check your credentials and try again."
    ))
  }

  connection
}

# min and max functions that return NA if all values are NA.
safe_min <- function(x) {
  if (all(is.na(x))) {
    x[1]
  } else {
    min(x, na.rm = TRUE)
  }
}
safe_max <- function(x) {
  if (all(is.na(x))) {
    x[1]
  } else {
    max(x, na.rm = TRUE)
  }
}

#' @title Select Columns from a Database Form Table
#' @description
#'   Retrieves a form table from the VHIO database and keeps a standardized
#'   set of identifier columns (`dem_sap`, `project`, `event_arm` and, when
#'   available, `repeating_instance`) alongside the variables requested in
#'   `...`. A `form` column recording the source table name is added after
#'   `event_arm`.
#'   If the source table has no `repeating_instance` column (i.e. the form is
#'   not a repeating instrument), a `repeating_instance` column set to `0` is
#'   added after `form`.
#'
#' @param tbl A single string with the name of the database table (form) to
#'   query.
#' @param ... Column names or tidy-select expressions with the additional
#'   variables to select, passed on to [dplyr::select()].
#' @param .connection A `DBIConnection` object to the database, as returned by
#'   `connect_to_vhio_database()`.
#'
#' @return A lazy [dplyr::tbl()] backed by the database with, in column order:
#'   \describe{
#'     \item{dem_sap}{Patient identifier.}
#'     \item{project}{Project the record belongs to.}
#'     \item{event_arm}{Event arm of the record.}
#'     \item{form}{Name of the source table (i.e. `tbl`).}
#'     \item{repeating_instance}{Repeating instance of the form, from the
#'       source table if present, otherwise `0`.}
#'     \item{...}{The additional columns requested via `...`.}
#'   }
#'   Identifier columns absent from the source table are silently dropped by
#'   [tidyselect::any_of()].
#'
#' @keywords internal
select_tbl <-
  function(tbl, ..., .connection) {
    result_v0 <-
      dplyr::tbl(.connection, tbl) |>
      dplyr::select(
        tidyselect::any_of(c(
          "dem_sap",
          "project",
          "event_arm",
          "repeating_instance"
        )),
        ...
      ) |>
      dplyr::mutate(
        form = tbl,
        .after = "event_arm"
      )

    if (!any(colnames(result_v0) == "repeating_instance")) {
      result <-
        result_v0 |>
        dplyr::mutate(
          repeating_instance = 0,
          .after = "form"
        )
    } else {
      result <- result_v0
    }
  }


filter_issues <- function(verified_data, issue_text) {
  # Columnas form*/repeating_instance*, en el orden en que vengan
  # (grep conserva el orden de las columnas del data frame)
  id_cols <- grep(
    "^(form|repeating_instance)(_.*)?$",
    names(verified_data),
    value = TRUE
  )

  verified_data |>
    dplyr::filter_out(.data$.ok) |>
    dplyr::mutate(
      issue = glue::glue(issue_text)
    ) |>
    dplyr::select(
      "dem_sap",
      "project",
      "event_arm",
      tidyselect::all_of(id_cols),
      "issue"
    ) |>
    dplyr::select(
      -dplyr::where(~ all(. == 0))
    )
}
