# vhiocerberus 0.1.0

First release of vhiocerberus, a package that runs automated quality checks on
the databases hosted in the VHIO data lake. This version covers the
OncoClinical database.

## Functions

* `verify_oncoclinical_data()` connects to the OncoClinical and OncoPharma
  databases and runs every verification enabled for a project. It can verify
  all the patients of the project or a subset (`patients`). A verification that
  fails to run does not stop the others: it is reported and its result is
  `NULL`. Patients without a record in the project are ignored with a warning.
  It returns a tibble with one row per verification, its number of issues and
  the issues found.

  The package code contains no credentials or server address. The arguments
  `user`, `password` and `server` default to `NULL`, in which case they are read
  from the environment variables `VHIO_SQL_SERVER_USER`,
  `VHIO_SQL_SERVER_PASSWORD` and `VHIO_SQL_SERVER_IP` (e.g. defined in
  `~/.Renviron`). An explicit argument takes precedence. A missing setting
  aborts before any connection is opened.

* `report_oncoclinical_verifications()` writes the result of
  `verify_oncoclinical_data()` to an Excel workbook, with a project
  information sheet, a summary of the verifications, a sheet with all the issues
  ordered by patient, and one sheet per verification with issues. The summary
  links to each verification sheet and each sheet links back to the summary.

* `oncoclinical_projects()` lists the projects for which verifications are
  available: `breast`, `crc`, `ge`, `larc_net`, `prostate`, `sarcomas`, `uitm`
  and `upgi`.

## Data

* `oncoclinical_verifications` is a table with the 29 verification rules
  (28 of plausibility and 1 of completeness), their description and the
  projects to which each one applies.

## Other

* Attaching the package prints a startup banner with the package version.
