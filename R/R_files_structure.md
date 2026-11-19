# R files structure

Architecture map of vhiocerberus. Each function lists its internal
dependencies first and then the packages it uses.

## **oncoclinical_verification_runner**

Entry points to run the verifications of a project (oncoclinical.R).

-   verify_oncoclinical_data [oncoclinical_projects, vhio_credentials, connect_to_vhio_database, warn_patients_not_in_project, run_verification, drop_zero_id_columns, cli, DBI, dplyr, purrr, rlang]
-   oncoclinical_projects []
-   run_verification [rlang]
-   drop_zero_id_columns [dplyr, tidyselect]
-   warn_patients_not_in_project [select_oncoclinical_tbl, filter_patients, cli, dplyr]

## **oncoclinical_report**

Excel report of the verification results (oncoclinical.R).

-   report_oncoclinical_verifications [issues_by_sap, dplyr, here, openxlsx, stats, stringr, tibble]
-   issues_by_sap [dplyr, purrr, stringr, tidyr, tidyselect]

## **database_access**

Settings and connections to the VHIO data lake (helpers.R).

-   resolve_setting [cli]
-   vhio_credentials [resolve_setting]
-   connect_to_vhio_database [cli, odbc]

## **oncoclinical_data_selection**

Retrieval and filtering of OncoClinical data (helpers.R).

-   select_oncoclinical_tbl [dplyr, tidyselect]
-   oncoclinical_select [select_oncoclinical_tbl, filter_patients, DBI, dplyr, lubridate, purrr, stringr, tidyr]
-   filter_patients [dplyr, rlang]
-   get_oncoclinical_date_variables [cli, DBI]
-   pivot_dates [dplyr, lubridate, purrr, tidyr]
-   filter_issues [dplyr, glue, tidyselect]

## **treatment_ordering**

Chronological ordering of antineoplastic treatments (helpers.R, ocv_helpers.R).

-   extract_ordered_treatment [select_oncoclinical_tbl, filter_patients, insert_by_reference_date, dplyr, lubridate, stringr]
-   insert_by_reference_date [dplyr]
-   add_next_setting_date [dplyr]
-   ocv_next_setting_date_order [extract_ordered_treatment, add_next_setting_date, filter_issues, dplyr, rlang, stringr]

## **verifications_completeness**

Completeness verifications, named ocv_cm<number> (oncoclinical_completeness.R).

-   ocv_cm1 [extract_ordered_treatment, filter_issues, dplyr, lubridate]

## **verifications_plausibility**

Plausibility verifications, named ocv_pl<number> (oncoclinical_plausibility.R).

-   ocv_pl1 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl2 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl3 [oncoclinical_select, filter_issues, dplyr, stringr]
-   ocv_pl4 [ocv_next_setting_date_order]
-   ocv_pl5 [ocv_next_setting_date_order]
-   ocv_pl6 [ocv_next_setting_date_order]
-   ocv_pl7 [extract_ordered_treatment, add_next_setting_date, filter_issues, dplyr, stringr]
-   ocv_pl8 [extract_ordered_treatment, add_next_setting_date, filter_issues, dplyr, stringr]
-   ocv_pl9 [extract_ordered_treatment, filter_issues, dplyr]
-   ocv_pl10 [get_oncoclinical_date_variables, oncoclinical_select, pivot_dates, filter_issues, dplyr, lubridate]
-   ocv_pl11 [get_oncoclinical_date_variables, oncoclinical_select, pivot_dates, filter_issues, dplyr]
-   ocv_pl12 [oncoclinical_select, pivot_dates, filter_issues, dplyr]
-   ocv_pl13 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl14 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl15 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl16 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl17 [oncoclinical_select, filter_issues, dplyr, stats]
-   ocv_pl18 [oncoclinical_select, filter_issues, dplyr, stats]
-   ocv_pl19 [oncoclinical_select, filter_issues, dplyr, stats]
-   ocv_pl20 [oncoclinical_select, filter_issues, dplyr, stats]
-   ocv_pl21 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl22 [get_oncoclinical_date_variables, oncoclinical_select, pivot_dates, filter_issues, dplyr, stringr]
-   ocv_pl23 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl24 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl25 [oncoclinical_select, filter_issues, dplyr, stringr]
-   ocv_pl26 [oncoclinical_select, filter_issues, dplyr, stringr]
-   ocv_pl27 [oncoclinical_select, filter_issues, dplyr]
-   ocv_pl28 [oncoclinical_select, filter_issues, dplyr, stringr]

## **package_startup**

Startup banner shown when the package is attached (zzz.R).

-   .onAttach [.cerberus_banner, utils]
-   .cerberus_banner [cli]
