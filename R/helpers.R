# Function to connect to the VHIO database using ODBC
# usert: Username for the database connection
# password: Password for the database connection
# database_name: Name of the database to connect to (e.g., "OncoClinical",
# "OncoPharma", "Prescreening_NEW")
# connection_name: Name to assign to the connection object in the global
# environment
# silent_success: If TRUE, suppresses success messages; default is FALSE
connect_to_vhio_database <- function(
  user,
  password,
  database_name,
  connection_name,
  silent_success = FALSE
) {
  rlang::arg_match(
    database_name,
    values = c("OncoClinical", "OncoPharma", "Prescreening_NEW"),
    multiple = FALSE
  )

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

  if (!silent_success) {
    cli::cli_alert_success("Successfully connected to {.val {database_name}}.")
    cli::cli_alert_info("Connection name: {.val {connection_name}}.")
  }

  assign(
    connection_name,
    connection,
    envir = rlang::global_env()
  )
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
