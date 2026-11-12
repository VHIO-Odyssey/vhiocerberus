# Helpers shared by the OncoClinical verifications (ocv_*).

#' @title Check Date Order Against the Next Treatment Setting
#' @description
#'   Shared implementation of `ocv_pl4()`, `ocv_pl5()` and `ocv_pl6()`. For
#'   each treatment, compares `date_var` with the same date in the next
#'   setting that has it present (see `add_next_setting_date()`) and reports
#'   two kinds of issues:
#'   \itemize{
#'     \item `date_var` is after the next setting's `date_var`.
#'     \item `date_var` is equal to the next setting's `date_var`.
#'   }
#'   Records with a missing `date_var` or no next setting are never reported.
#'
#' @param project A single string with the project name.
#' @param patients A vector of patient identifiers (`dem_sap`) to verify, or
#'   `NULL` for all the patients of the project.
#' @param connection A `DBIConnection` object to the database, as returned by
#'   `connect_to_vhio_database()`.
#' @param date_var A single string with the date column to verify: one of
#'   `"ttm_startdate"`, `"ttm_enddate"` or `"ttm_pddate"`.
#'
#' @return A tibble of issues as returned by `filter_issues()`.
#'
#' @seealso `extract_ordered_treatment()`, `add_next_setting_date()`
#' @keywords internal
ocv_next_setting_date_order <- function(
  project,
  patients,
  connection,
  date_var = c("ttm_startdate", "ttm_enddate", "ttm_pddate")
) {
  date_var <- rlang::arg_match(date_var)
  next_var <- paste0("next_", date_var)

  ttm_and_next_ttm <-
    extract_ordered_treatment(project, patients, connection) |>
    add_next_setting_date(date_var)

  # Missing dates (current or next) are not issues
  any_missing <-
    is.na(ttm_and_next_ttm[[date_var]]) | is.na(ttm_and_next_ttm[[next_var]])

  is_after <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        .data$next_instance,
        sep = ", "
      ),
      .ok = .data[[date_var]] <= .data[[next_var]] | any_missing
    ) |>
    filter_issues(
      paste0(
        "{setting_label} ",
        date_var,
        " ({",
        date_var,
        "}) is after ",
        "{next_setting} ",
        date_var,
        " ({",
        next_var,
        "})"
      )
    )

  is_equal <-
    ttm_and_next_ttm |>
    dplyr::mutate(
      repeating_instance = stringr::str_c(
        .data$repeating_instance,
        .data$next_instance,
        sep = ", "
      ),
      .ok = .data[[date_var]] != .data[[next_var]] | any_missing
    ) |>
    filter_issues(
      paste0(
        "{setting_label} ",
        date_var,
        " ({",
        date_var,
        "}) is equal to ",
        "{next_setting} ",
        date_var,
        " ({",
        next_var,
        "})"
      )
    )

  dplyr::bind_rows(is_after, is_equal)
}


#' @title Get the Date Variables Defined in the OncoClinical Database
#' @description
#'   Queries the database catalogue (`INFORMATION_SCHEMA.COLUMNS`) and returns
#'   the names of all the columns defined with the SQL Server `date` type. Use
#'   it to discover which variables can be treated as dates without hard-coding
#'   their names.
#'
#' @param connection A `DBIConnection` object to the SQL Server database, as
#'   returned by `connect_to_vhio_database()`.
#'
#' @return A character vector with the alphabetically sorted names of the
#'   columns whose SQL Server type is `date`. Returns a zero-length character
#'   vector if the database has no such columns.
#'
#' @details
#'   Before querying, the function checks that `connection` is connected to the
#'   `OncoClinical` database (via `DB_NAME()`) and aborts with an error
#'   otherwise.
#'
#'   All the tables and views of the database are inspected. The database is a
#'   copy of a REDCap project, where variable names are unique across
#'   instruments, so each name is expected to appear in a single table and no
#'   de-duplication is done. Columns of any other type (`datetime`, `time`, or
#'   dates stored as text such as `varchar`) are not included. The queries use
#'   SQL Server functions and data type names, so they are not guaranteed to
#'   work with other database engines.
#'
#' @keywords internal
get_oncoclinical_date_variables <- function(connection) {
  database_name <- DBI::dbGetQuery(connection, "SELECT DB_NAME() AS db")$db

  if (!identical(database_name, "OncoClinical")) {
    cli::cli_abort(c(
      "x" = "{.arg connection} must be connected to the {.val OncoClinical} database.",
      "i" = "It is connected to {.val {database_name}}."
    ))
  }

  DBI::dbGetQuery(
    connection,
    "
  SELECT COLUMN_NAME
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE DATA_TYPE = 'date'
  ORDER BY COLUMN_NAME
"
  )$COLUMN_NAME
}
