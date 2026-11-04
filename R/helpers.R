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

    result
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
      "event_arm",
      tidyselect::all_of(id_cols),
      "issue"
    ) |>
    dplyr::select(
      # Esto elimina la columna de eventos e instancias si todas son 0
      -dplyr::where(~ all(. == 0))
    ) |>
    dplyr::mutate(
      dem_sap = as.character(.data$dem_sap)
    )
}


#' @title Extract and Chronologically Order Antineoplastic Treatments
#' @description
#'   Retrieves the `antineoplasic_therapy` form from the VHIO database for a
#'   given project, restricted to records with `ttm_stat == "Yes"`, and orders
#'   each patient's treatments chronologically. A `reference_date` is derived
#'   per record as the first non-missing of `ttm_startdate`, `ttm_enddate` and
#'   `ttm_pddate` (parsed with [lubridate::ymd()]).
#'
#'   The ordering proceeds in two stages. First, records other than adjuvant
#'   therapies after M1 surgery (`ttm_ad_int == "After M1 surgery"`) are
#'   arranged by setting — neoadjuvant, adjuvant, then metastatic lines in
#'   ascending line-number order — and, within each setting, by
#'   `reference_date`. Metastatic line numbers come from a purely numeric
#'   parse of `ttm_met_line_num`. Second, adjuvant-to-M1 records (labelled
#'   `"Adjuvant Metastatic"`) are reinserted into their patient's sequence at
#'   the position implied by their `reference_date`, preserving the relative
#'   order of the remaining records; those with unknown `reference_date` are
#'   dropped. Each record finally receives an `order` column numbering the
#'   sequence within patient (`dem_sap`) and `event_arm`, since a patient may
#'   have more than one cancer.
#'
#' @param project A single string with the project name used to filter the
#'   `project` column of the source table.
#' @param connection A `DBIConnection` object to the database, as returned by
#'   `connect_to_vhio_database()`.
#'
#' @return A collected tibble ordered by `dem_sap`, `event_arm` and treatment
#'   order, with columns:
#'   \describe{
#'     \item{dem_sap, project, event_arm}{Record identifiers.}
#'     \item{form, repeating_instance}{Source form identifiers.}
#'     \item{order}{Integer sequence number of the treatment within
#'       `dem_sap` and `event_arm`, in chronological order.}
#'     \item{setting_label}{Human-readable label: `"Neoadjuvant"`,
#'       `"Adjuvant"`, `"Adjuvant Metastatic"` for adjuvant therapy after M1
#'       surgery, or `"<line>L"` (e.g. `"2L"`) for metastatic lines.}
#'     \item{reference_date}{Date used for chronological ordering: first
#'       non-missing of `ttm_startdate`, `ttm_enddate` and `ttm_pddate`;
#'       `NA` if all three are missing.}
#'     \item{ttm_startdate, ttm_enddate, ttm_pddate}{Treatment start, end and
#'       progression dates, as stored in the source table.}
#'   }
#'   Returns a tibble with zero rows if the project has no records with
#'   `ttm_stat == "Yes"`.
#'
#' @seealso [select_tbl()]
#' @keywords internal
extract_ordered_treatment <- function(project, connection) {
  ttm_raw <-
    select_tbl(
      "antineoplasic_therapy",
      "ttm_stat",
      "ttm_setting",
      "ttm_met_line_num",
      "ttm_ad_int",
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate",
      .connection = connection
    ) |>
    dplyr::filter(
      .data$project == .env$project,
      .data$ttm_stat == "Yes"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      reference_date = dplyr::case_when(
        !is.na(.data$ttm_startdate) ~ .data$ttm_startdate,
        !is.na(.data$ttm_enddate) ~ .data$ttm_enddate,
        !is.na(.data$ttm_pddate) ~ .data$ttm_pddate,
        .default = NA_character_
      ) |>
        lubridate::ymd(),
      ttm_startdate = lubridate::ymd(.data$ttm_startdate),
      ttm_enddate = lubridate::ymd(.data$ttm_enddate),
      ttm_pddate = lubridate::ymd(.data$ttm_pddate)
    )

  # Método de ubicación:
  # La idea general es ordenar por setting
  #1 Se extraen las adyuvancias a M1 (típico de Colorectal) ya que no son
  # ubicables por su setting (una adj M1 puede caer entre cualquiera lineas)
  #2 Se ordena por setting: Neoadjuvant, Adjuvant, Metastatic (por línea)
  #3 Dentro de cada setting se ordena por fecha de referencia
  #4 Se reintroducen las adyuvancias a M1 a partir de su fecha de referencia

  ttm_ordered_v0 <-
    ttm_raw |>
    dplyr::filter_out(ttm_ad_int == "After M1 surgery") |>
    dplyr::mutate(
      # versión numérica de ttm_met_line_num2
      ttm_met_line_num2 = dplyr::if_else(
        stringr::str_detect(.data$ttm_met_line_num, "^\\d+$"),
        as.numeric(.data$ttm_met_line_num) |> suppressWarnings(),
        NA_real_
      ),
      num_order = dplyr::case_when(
        !is.na(.data$ttm_met_line_num2) ~ .data$ttm_met_line_num2,
        .data$ttm_setting == "Neoadjuvant" ~ -1,
        .data$ttm_setting == "Adjuvant" ~ 0,
        .default = NA
      ),
      setting_label = dplyr::case_when(
        .data$ttm_setting %in% c("Neoadjuvant", "Adjuvant") ~ .data$ttm_setting,
        !is.na(.data$ttm_met_line_num) ~ stringr::str_c(
          .data$ttm_met_line_num,
          "L"
        ),
        .default = NA_character_
      )
    ) |>
    dplyr::arrange(
      .data$dem_sap,
      .data$event_arm,
      .data$num_order,
      .data$reference_date
    ) |>
    dplyr::select(
      "dem_sap",
      "project",
      "event_arm",
      "form",
      "repeating_instance",
      "setting_label",
      "reference_date",
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate"
    )

  ttm_adj_m1 <-
    ttm_raw |>
    dplyr::filter(ttm_ad_int == "After M1 surgery") |>
    dplyr::filter_out(is.na(.data$reference_date)) |>
    dplyr::mutate(
      setting_label = "Adjuvant Metastatic"
    ) |>
    dplyr::select(
      "dem_sap",
      "project",
      "event_arm",
      "form",
      "repeating_instance",
      "setting_label",
      "reference_date",
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate"
    ) |>
    dplyr::arrange(
      .data$dem_sap,
      .data$reference_date
    )

  if (nrow(ttm_adj_m1) > 0) {
    # Insertar ttm_adj_m1 en ttm_ordered_v0 sin alterar el orden de v0:
    # cada fila de m1 se intercala dentro de su paciente y event_arm (un paciente
    # puede tener más de un cáncer), tras la última fila de v0 con reference_date
    # conocida <= la suya (o al inicio del bloque si es anterior a todas)
    v0 <-
      ttm_ordered_v0 |>
      dplyr::mutate(.ord = dplyr::row_number())

    m1_pos <-
      ttm_adj_m1 |>
      dplyr::mutate(.mrow = dplyr::row_number()) |>
      dplyr::left_join(
        v0 |>
          dplyr::select(
            "dem_sap",
            "event_arm",
            ".ord",
            .ref = "reference_date"
          ),
        by = c("dem_sap", "event_arm"),
        relationship = "many-to-many"
      ) |>
      dplyr::group_by(.data$.mrow) |>
      dplyr::summarise(
        .pos = if (all(is.na(.data$.ord))) {
          max(v0$.ord) + .data$.mrow[1] # paciente-event_arm nuevo: al final
        } else if (is.na(.data$reference_date[1])) {
          max(.data$.ord) + 0.5 # fecha m1 desconocida: al final del paciente
        } else {
          # última fila de v0 con reference_date conocida y <= a la de m1
          prev <- !is.na(.data$.ref) & .data$.ref <= .data$reference_date[1]
          if (any(prev)) max(.data$.ord[prev]) + 0.5 else min(.data$.ord) - 0.5
        },
        .groups = "drop"
      )

    ttm_ordered <-
      dplyr::bind_rows(
        v0 |>
          dplyr::mutate(
            .pos = as.numeric(.data$.ord),
            .mrow = 0L,
            .src = "v0"
          ),
        ttm_adj_m1 |>
          dplyr::mutate(.mrow = dplyr::row_number(), .src = "m1") |>
          dplyr::left_join(m1_pos, by = ".mrow")
      ) |>
      dplyr::arrange(.data$.pos, .data$.mrow) |>
      dplyr::select(-".ord", -".pos", -".mrow", -".src") |>
      dplyr::mutate(
        order = dplyr::row_number(),
        .by = c("dem_sap", "event_arm"),
        .after = "repeating_instance"
      )
  } else {
    ttm_ordered <- ttm_ordered_v0 |>
      dplyr::mutate(
        order = dplyr::row_number(),
        .by = c("dem_sap", "event_arm"),
        .after = "repeating_instance"
      )
  }

  ttm_ordered
}
