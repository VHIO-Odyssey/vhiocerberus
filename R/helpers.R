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

#' @title Select Columns from an OncoClinical Form Table
#' @description
#'   Retrieves a form table from the OncoClinical database and keeps a
#'   standardized set of identifier columns (`dem_sap`, `project`, `event_arm` and, when
#'   available, `repeating_instance`) alongside the variables requested in
#'   `...`. A `form` column recording the source table name is added after
#'   `event_arm`.
#'   If the source table has no `repeating_instance` column (i.e. the form is
#'   not a repeating instrument), a `repeating_instance` column set to `0` is
#'   added after `form`.
#'
#'   This function only works with the OncoClinical database: it assumes the
#'   OncoClinical table structure (`dem_sap`, `project`, `event_arm` and
#'   `repeating_instance` identifier columns). It is not intended for other
#'   VHIO databases such as OncoPharma or Prescreening_NEW.
#'
#' @param tbl A single string with the name of the OncoClinical table (form) to
#'   query.
#' @param ... Column names or tidy-select expressions with the additional
#'   variables to select, passed on to [dplyr::select()].
#' @param .connection A `DBIConnection` object to the OncoClinical database, as
#'   returned by `connect_to_vhio_database()` with
#'   `database_name = "OncoClinical"`.
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
select_oncoclinical_tbl <-
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

#' @title Select Variables from OncoClinical Across Forms
#' @description
#'   Locates, in the OncoClinical database, every table that contains the
#'   requested variables and returns their values for the given project and
#'   patients. Tables are found by matching the requested names against
#'   `INFORMATION_SCHEMA.COLUMNS`, so the user does not need to know which form
#'   each variable belongs to. For each table found, the variables are
#'   retrieved with `select_oncoclinical_tbl()` (so the identifier columns
#'   `dem_sap`, `project`, `event_arm`, `form` and `repeating_instance` are
#'   always included), filtered to `.project` and `.patients`, and collected
#'   into memory.
#'
#'   Only works with the OncoClinical database.
#'
#' @param ... Variable (column) names to retrieve, as strings or character
#'   vectors. Names that do not exist in any table are silently ignored.
#' @param .project A single string with the project name used to filter the
#'   `project` column.
#' @param .patients A vector of patient identifiers (`dem_sap`) to keep. If
#'   `NULL` (the default), no filter on patients is applied, i.e. all the
#'   patients of `.project` are kept.
#' @param .connection A `DBIConnection` object to the OncoClinical database, as
#'   returned by `connect_to_vhio_database()` with
#'   `database_name = "OncoClinical"`.
#'
#' @return If the variables are found in a single table, a collected tibble
#'   with the columns described in `select_oncoclinical_tbl()`. Otherwise, an
#'   unnamed list of collected tibbles, one per table, in the order the tables
#'   are returned by the database metadata query. A variable present in more
#'   than one table is retrieved from each of them.
#'
#' @seealso `select_oncoclinical_tbl()`
#' @keywords internal
oncoclinical_select <- function(
  ...,
  .project,
  .patients = NULL,
  .connection
) {
  vars_metadata <-
    DBI::dbGetQuery(
      .connection,
      "
  SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, DATA_TYPE, ORDINAL_POSITION
  FROM INFORMATION_SCHEMA.COLUMNS
  ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION"
    ) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::filter(.data$column_name %in% c(...)) |>
    dplyr::select("table_name", "column_name", "data_type") |>
    tidyr::nest(variables = c("column_name", "data_type"))

  selection <-
    purrr::pmap(
      vars_metadata,
      function(table_name, variables) {
        result <-
          select_oncoclinical_tbl(
            table_name,
            variables$column_name,
            .connection = .connection
          ) |>
          dplyr::filter(.data$project == .project) |>
          filter_patients(.patients) |>
          dplyr::collect()

        date_vars <- variables |>
          dplyr::filter(.data$data_type == "date") |>
          dplyr::pull("column_name")

        if (length(date_vars) > 0) {
          result <-
            result |>
            dplyr::mutate(
              dplyr::across(dplyr::all_of(date_vars), lubridate::ymd)
            )

          return(result)
        }

        result
      }
    )
  if (length(selection) == 1) {
    return(selection[[1]])
  }
  selection
}

#' @title Filter a Table by Patients
#' @description
#'   Keeps the rows of `data` (a data frame or a lazy database table) whose
#'   `dem_sap` is in `patients`. If `patients` is `NULL`, `data` is returned
#'   unchanged, so no patient filter is sent to the database. Identifiers that
#'   cannot be converted to numbers are silently dropped (and never match rows
#'   with a missing `dem_sap`).
#'
#' @param data A data frame or lazy [dplyr::tbl()] with a `dem_sap` column.
#' @param patients A vector (numeric, character or factor) of patient
#'   identifiers, or `NULL` for no filter.
#'
#' @return `data`, filtered if `patients` is not `NULL`.
#' @keywords internal
filter_patients <- function(data, patients) {
  if (is.null(patients)) {
    return(data)
  }

  # as.character() evita que un factor se convierta en sus códigos internos
  patients <- as.numeric(as.character(patients)) |>
    suppressWarnings()
  # Los id's no numéricos (NA) no deben coincidir con dem_sap = NA
  patients <- patients[!is.na(patients)]

  dplyr::filter(data, .data$dem_sap %in% .env$patients)
}

#' @title Extract the Issues from a Verified Data Frame
#' @description
#'   Final step of the OncoClinical verifications (`ocv_*`). Takes a data frame
#'   in which each row has been checked and flagged in a logical `.ok` column,
#'   keeps the rows that failed the check, and builds a human-readable `issue`
#'   message for each of them. Rows where `.ok` is `NA` are not dropped and are
#'   therefore reported as issues.
#'
#' @param verified_data A data frame with a logical column `.ok` (`TRUE` when
#'   the record passes the verification) and the columns `dem_sap` and
#'   `event_arm`. Any column whose name is `form` or `repeating_instance`, or
#'   starts with `form_` or `repeating_instance_` (e.g. the suffixed columns
#'   resulting from a join), is kept as well.
#' @param issue_text A single string used as a [glue::glue()] template to build
#'   the message. It is evaluated within `verified_data`, so column names in
#'   braces are replaced by their values for each row (e.g.
#'   `"ttm_startdate ({ttm_startdate}) is after ttm_enddate ({ttm_enddate})"`).
#'
#' @return A tibble with one row per failed record and the columns `dem_sap`,
#'   `event_arm`, the `form*` / `repeating_instance*` columns (in their original
#'   order) and `issue`, a [glue::glue()] character column with the message.
#'   Zero rows if every record passes.
#'
#' @seealso `ocv_next_setting_date_order()`
#' @keywords internal
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
#'   parse of `ttm_met_line_num`. Second, two groups of records are
#'   reinserted into their patient's sequence at the position implied by their
#'   `reference_date` (see `insert_by_reference_date()`), preserving the
#'   relative order of the remaining records: adjuvant-to-M1 records
#'   (labelled `"Adjuvant Metastatic"`) and records with an unorderable
#'   setting (no numeric line and not neoadjuvant/adjuvant) that still have a
#'   `setting_label` and a `reference_date`. Records of either group with
#'   unknown `reference_date`, and unorderable records without
#'   `setting_label`, are dropped. Each record finally receives an `order` column numbering the
#'   sequence within patient (`dem_sap`) and `event_arm`, since a patient may
#'   have more than one cancer.
#'
#' @param project A single string with the project name used to filter the
#'   `project` column of the source table.
#' @param patients A vector of patient identifiers (`dem_sap`) to keep, or
#'   `NULL` to keep all the patients of the project.
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
#'       surgery, `"<line>L"` (e.g. `"2L"`) for metastatic lines, or the
#'       original `ttm_setting` for settings that cannot be ordered. For the
#'       reinserted records (adjuvant after M1 and unorderable settings), when
#'       a patient and `event_arm` have more than one separate run of the
#'       same label, the first run keeps the plain label and each later run
#'       is suffixed with its sequence number (e.g. `"Adjuvant Metastatic"`,
#'       `"Adjuvant Metastatic 2"`, `"Adjuvant Metastatic 3"`).}
#'     \item{reference_date}{Date used for chronological ordering: first
#'       non-missing of `ttm_startdate`, `ttm_enddate` and `ttm_pddate`;
#'       `NA` if all three are missing.}
#'     \item{ttm_startdate, ttm_enddate, ttm_pddate}{Treatment start, end and
#'       progression dates, as stored in the source table.}
#'   }
#'   Returns a tibble with zero rows if the project has no records with
#'   `ttm_stat == "Yes"`.
#'
#' @seealso `select_oncoclinical_tbl()`
#' @keywords internal
extract_ordered_treatment <- function(project, patients, connection) {
  ttm_raw <-
    select_oncoclinical_tbl(
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
    filter_patients(patients) |>
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
  #4 Se reintroducen las adyuvancias a M1 y los settings no ordenables (con
  # setting_label y fecha de referencia) a partir de su fecha de referencia

  ttm_ordered_v0 <-
    ttm_raw |>
    dplyr::filter_out(.data$ttm_ad_int == "After M1 surgery") |>
    dplyr::mutate(
      # versión numérica de ttm_met_line_num2
      ttm_met_line_num2 = dplyr::if_else(
        stringr::str_detect(.data$ttm_met_line_num, "^\\d+$"),
        as.numeric(.data$ttm_met_line_num) |> suppressWarnings(),
        NA_real_
      ),
      num_order = dplyr::case_when(
        .data$ttm_setting == "Neoadjuvant" ~ -1,
        .data$ttm_setting == "Adjuvant" ~ 0,
        !is.na(.data$ttm_met_line_num2) ~ .data$ttm_met_line_num2,
        .default = NA
      ),
      setting_label = dplyr::case_when(
        .data$ttm_setting %in% c("Neoadjuvant", "Adjuvant") ~ .data$ttm_setting,
        !is.na(.data$ttm_met_line_num) ~ stringr::str_c(
          .data$ttm_met_line_num,
          "L"
        ),
        !.data$ttm_setting %in%
          c(
            "Neoadjuvant",
            "Adjuvant",
            "Metastatic or Palliative"
          ) ~ .data$ttm_setting,
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
      "num_order",
      "setting_label",
      "reference_date",
      "ttm_startdate",
      "ttm_enddate",
      "ttm_pddate"
    )

  # settings extraños no ordenados.
  ttm_no_ordered <-
    ttm_ordered_v0 |>
    dplyr::filter(
      is.na(.data$num_order),
      # Han de tener fecha de referencia para reubicarlos después.
      !is.na(.data$reference_date),
      # Han de tener setting_label (si no lo tienen es que ttm_setting estaba
      # vacío o es un metastatico sin linea, ambos casos son un problema de
      # completeness)
      !is.na(.data$setting_label)
    ) |>
    dplyr::select(-"num_order")

  ttm_ordered_v1 <-
    ttm_ordered_v0 |>
    dplyr::filter_out(is.na(.data$num_order)) |>
    dplyr::select(-"num_order")

  ttm_adj_m1 <-
    ttm_raw |>
    dplyr::filter(.data$ttm_ad_int == "After M1 surgery") |>
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

  # Ambos grupos se ubican por fecha: se insertan en una sola pasada.
  # `.inserted` marca temporalmente las filas insertadas para numerar sus runs.
  ttm_ordered <-
    insert_by_reference_date(
      dplyr::mutate(ttm_ordered_v1, .inserted = FALSE),
      dplyr::bind_rows(ttm_adj_m1, ttm_no_ordered) |>
        dplyr::mutate(.inserted = TRUE)
    ) |>
    dplyr::mutate(
      order = dplyr::row_number(),
      .by = c("dem_sap", "event_arm"),
      .after = "repeating_instance"
    ) |>
    dplyr::mutate(
      run_id = dplyr::consecutive_id(.data$setting_label),
      .by = c("project", "dem_sap", "event_arm")
    ) |>
    # Runs de una misma etiqueta insertada dentro de paciente y event_arm
    dplyr::mutate(
      label_run = match(.data$run_id, unique(.data$run_id)),
      .by = c("project", "dem_sap", "event_arm", "setting_label", ".inserted")
    ) |>
    dplyr::mutate(
      # El primer run conserva la etiqueta; los siguientes llevan su número
      setting_label = dplyr::if_else(
        .data$.inserted & .data$label_run > 1,
        paste(.data$setting_label, .data$label_run),
        .data$setting_label
      )
    ) |>
    dplyr::select(-".inserted", -"run_id", -"label_run")

  ttm_ordered
}

#' @title Insert Rows into an Ordered Table by Reference Date
#' @description
#'   Intercalates the rows of `to_insert` into `base` without altering the
#'   relative order of `base`. Within each `dem_sap` and `event_arm` (a patient
#'   may have more than one cancer), each inserted row is placed after the last
#'   row of `base` whose `reference_date` is known and `<=` its own, or at the
#'   start of the block if it precedes all of them. Inserted rows with unknown
#'   `reference_date` go to the end of the patient block; rows of a
#'   `dem_sap`/`event_arm` absent from `base` are appended at the end.
#'   Inserted rows sharing a position keep chronological order.
#'
#' @param base A tibble already in its final order, with at least `dem_sap`,
#'   `event_arm` and `reference_date`.
#' @param to_insert A tibble with the same columns as `base` (extra columns
#'   are filled with `NA` in `base` rows).
#'
#' @return A tibble with the rows of `base` and `to_insert` and the columns of
#'   `base` (no helper columns).
#'
#' @seealso `extract_ordered_treatment()`
#' @keywords internal
insert_by_reference_date <- function(base, to_insert) {
  if (nrow(to_insert) == 0) {
    return(base)
  }

  base <- dplyr::mutate(base, .ord = dplyr::row_number())

  to_insert <-
    to_insert |>
    dplyr::arrange(.data$dem_sap, .data$event_arm, .data$reference_date) |>
    dplyr::mutate(.mrow = dplyr::row_number())

  insert_pos <-
    to_insert |>
    dplyr::select("dem_sap", "event_arm", ".mrow", "reference_date") |>
    dplyr::left_join(
      base |>
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
        max(base$.ord, 0) + .data$.mrow[1] # paciente-event_arm nuevo: al final
      } else if (is.na(.data$reference_date[1])) {
        max(.data$.ord) + 0.5 # fecha desconocida: al final del paciente
      } else {
        # última fila de base con reference_date conocida y <= a la insertada
        prev <- !is.na(.data$.ref) & .data$.ref <= .data$reference_date[1]
        if (any(prev)) max(.data$.ord[prev]) + 0.5 else min(.data$.ord) - 0.5
      },
      .groups = "drop"
    )

  dplyr::bind_rows(
    base |>
      dplyr::mutate(.pos = as.numeric(.data$.ord), .mrow = 0L),
    dplyr::left_join(to_insert, insert_pos, by = ".mrow")
  ) |>
    dplyr::arrange(.data$.pos, .data$.mrow) |>
    dplyr::select(-".ord", -".pos", -".mrow")
}

#' @title Add the Next Present Date After the Current Treatment Setting
#' @description
#'   Annotates every treatment record returned by
#'   `extract_ordered_treatment()` with the first record, in a later
#'   treatment setting of the same patient and cancer, that has a non-missing
#'   `date_var`: its date, its `repeating_instance` and its `setting_label`. It
#'   is useful to
#'   bound the time window of a setting by the moment a following one begins.
#'
#'   Within each patient and cancer (`dem_sap` and `event_arm`), consecutive
#'   records sharing the same `setting_label` are grouped into a run (the
#'   numeric suffixes, e.g. `"Adjuvant Metastatic 2"`, produced by
#'   `extract_ordered_treatment()` for the later runs of reinserted settings,
#'   keep them apart in the labels). For each run, the next record is the one with the lowest `order`
#'   among the records of later runs whose `date_var` is not missing. The same
#'   next record is assigned to all records of the run. Records with no such
#'   later record (e.g. those in the last run) receive `NA`.
#'
#' @param extract_ordered_treatment_data A tibble as returned by
#'   `extract_ordered_treatment()`. It must contain the columns `dem_sap`,
#'   `event_arm`, `setting_label`, `order`, `repeating_instance` and the column
#'   named by
#'   `date_var`. Only these columns are used; any other column is preserved
#'   untouched.
#' @param date_var A single string with the name of a date column of
#'   `extract_ordered_treatment_data` from which the next present date is
#'   taken (typically one of `"reference_date"`, `"ttm_startdate"`,
#'   `"ttm_enddate"` or `"ttm_pddate"`, all produced by
#'   `extract_ordered_treatment()`).
#'
#' @return An ungrouped tibble with all the rows and columns of
#'   `extract_ordered_treatment_data` plus three new columns, which are `NA`
#'   when there is no later record with a present `date_var` in the same
#'   `dem_sap` and `event_arm`:
#'   \describe{
#'     \item{next_<date_var>}{Value of `date_var` in the next record with a
#'       present date in a later setting (e.g. `next_reference_date` when
#'       `date_var = "reference_date"`).}
#'     \item{next_instance}{Value of `repeating_instance` (the instance
#'       number in the database) of that record.}
#'     \item{next_setting}{`setting_label` of that record.}
#'   }
#'
#' @details
#'   Settings whose records all have a missing `date_var` are skipped, so
#'   `next_setting` is not necessarily the setting immediately following the
#'   current one. Within the next setting, records without a date are also
#'   skipped, so `next_instance` may not be the first record of that setting.
#'
#'   Runs are computed within each `dem_sap` and `event_arm` combination,
#'   consistent with how `order` is numbered, so the sequences of different
#'   cancers of the same patient are never mixed. The function relies on the
#'   row order of the input (as returned by `extract_ordered_treatment()`) to
#'   identify consecutive records with the same `setting_label`.
#'
#' @seealso `extract_ordered_treatment()`
#' @keywords internal
add_next_setting_date <- function(extract_ordered_treatment_data, date_var) {
  data_run <-
    extract_ordered_treatment_data |>
    dplyr::mutate(
      run = dplyr::consecutive_id(.data$setting_label),
      .by = c("dem_sap", "event_arm")
    )

  # Registros con fecha presente, candidatos a "siguiente"
  present <-
    data_run |>
    dplyr::filter_out(is.na(.data[[date_var]])) |>
    dplyr::mutate(
      .data$dem_sap,
      .data$event_arm,
      next_run = .data$run,
      "next_{date_var}" := .data[[date_var]],
      next_instance = .data$repeating_instance,
      next_order = .data$order,
      next_setting = .data$setting_label,
      .keep = "none"
    )

  # Para cada run, el primer registro (menor `order`) con fecha presente en un
  # run posterior
  next_by_run <-
    data_run |>
    dplyr::distinct(.data$dem_sap, .data$event_arm, .data$run) |>
    dplyr::left_join(
      present,
      by = dplyr::join_by("dem_sap", "event_arm", "run" < "next_run")
    ) |>
    dplyr::slice_min(
      .data$next_order,
      by = c("dem_sap", "event_arm", "run"),
      n = 1,
      with_ties = FALSE,
      na_rm = FALSE
    ) |>
    dplyr::select(-"next_run", -"next_order")

  data_run |>
    dplyr::left_join(next_by_run, by = c("dem_sap", "event_arm", "run")) |>
    dplyr::select(-"run")
}


#' @title Pivot Date Columns to Long Format
#' @description
#'   Reshapes a list of tibbles, such as the one returned by
#'   `oncoclinical_select()` when the requested variables live in several
#'   tables, into a single long tibble with one row per non-missing date.
#'   In each tibble, every column of class `Date` is pivoted into a `variable`
#'   (column name) and `date` (value) pair with [tidyr::pivot_longer()]. The
#'   resulting tibbles are then stacked with [dplyr::bind_rows()] and the rows
#'   with a missing `date` are dropped.
#'
#' @param data A list of data frames (e.g. tibbles), each with at least one
#'   column of class `Date`. A single data frame is not supported: it would be
#'   iterated column by column.
#'
#' @return A tibble with all the non-`Date` columns of the input tibbles (those
#'   not shared between tibbles are filled with `NA`), plus:
#'   \describe{
#'     \item{variable}{Name of the original date column.}
#'     \item{date}{Date value, never `NA`.}
#'   }
#'
#' @seealso `oncoclinical_select()`
#' @keywords internal
pivot_dates <- function(data) {
  purrr::map(
    data,
    ~ tidyr::pivot_longer(
      .,
      dplyr::where(lubridate::is.Date),
      names_to = "variable",
      values_to = "date"
    )
  ) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::filter_out(is.na(date))
}
