# vhiocerberus

<!-- badges: start -->
<!-- badges: end -->

vhiocerberus runs automated verification and quality control checks on the
databases hosted in the VHIO (Vall d'Hebron Institute of Oncology) data lake,
to help detect data quality issues and ensure the integrity of the information
stored there.

The current version covers the **OncoClinical** database. It runs a set of
verification rules for a given project and writes the issues found to an Excel
report that data curators can review.

> **Note:** the package connects to an internal VHIO SQL Server. It is only
> usable from the VHIO network (or VPN) and with valid database credentials.
> The verification rules, however, are documented in the package and can be
> inspected by anyone.

## Requirements

* R >= 4.1.0.
* A SQL Server ODBC driver named `SQL Server` (installed by default on
  Windows).
* Access to the VHIO network or VPN and an account with read permissions on the
  OncoClinical and OncoPharma databases.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("<github-user>/vhiocerberus")
```

## Configuration

The package contains no credentials or server addresses. The user, password
and address of the SQL Server are read from these environment variables, which
you can define in your `~/.Renviron` (for example with
`usethis::edit_r_environ()`) and then restart R:

```
VHIO_SQL_SERVER_USER=<your_user>
VHIO_SQL_SERVER_PASSWORD=<your_password>
VHIO_SQL_SERVER_IP=<server_address>
```

Alternatively, pass `user`, `password` and `server` to
`verify_oncoclinical_data()`. An explicit argument takes precedence over the
environment variable. Avoid typing the password in scripts.

## Usage

The workflow has two steps: run the verifications of a project and write the
report.

``` r
library(vhiocerberus)

# Projects for which verifications are available
oncoclinical_projects()

# Run the verifications of a project (credentials from ~/.Renviron)
verifications <- verify_oncoclinical_data(project = "breast")

# Write the Excel report (in the project root, or in `folder`)
report_oncoclinical_verifications(verifications, folder = "reports")
```

To verify only some patients, pass their identifiers (`dem_sap`) in `patients`.
A patient without a record in the project is ignored with a warning, and the
report file name gets a `_subset` suffix:

``` r
verify_oncoclinical_data(
  project = "breast",
  patients = c("<id_1>", "<id_2>")
)
```

If a verification fails to run, the others continue: the failure is reported
and its `n_issues` is `NA`.

## Verifications

The rules are stored in the `oncoclinical_verifications` dataset, which has
one row per rule: its identifier (`verif_id`), its `type`, a description
(`verification`), optional `details`, and one 0/1 column per project saying
whether the rule applies to it.

``` r
oncoclinical_verifications
```

There are 29 rules (28 of plausibility and 1 of completeness). The number that
applies to each project is:

| Project    | Tumor group       | Rules |
|:-----------|:------------------|------:|
| `breast`   | Breast            |    20 |
| `crc`      | Colorectal        |    19 |
| `ge`       | Gastroesophageal  |    18 |
| `larc_net` | LARC / NET        |    17 |
| `prostate` | Prostate          |    19 |
| `sarcomas` | Sarcomas          |    15 |
| `uitm`     | UITM              |    15 |
| `upgi`     | Upper GI          |    17 |

## The Excel report

`report_oncoclinical_verifications()` creates the file
`<project>_oncoclinical_verifications[_subset]_<YYYYmmdd_HHMM>.xlsx` with these
sheets:

| Sheet                   | Content                                                              |
|:------------------------|:---------------------------------------------------------------------|
| `project_info`          | Project, patients verified and date-time of the verification.        |
| `verifications_summary` | One row per rule with its number of issues, linked to its sheet.     |
| `all_issues`            | All the issues, one row per issue, ordered by patient.               |
| `<verif_id>`            | One sheet per rule with issues, with a link back to the summary.     |

The report contains patient-level clinical data. Handle it according to your
data protection rules and do not commit it to version control.
