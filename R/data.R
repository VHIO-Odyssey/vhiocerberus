#' Oncoclinical data-verification rules
#'
#' A table of data-verification rules used to flag inconsistencies in the OncoClinical database, together with the tumor groups to which each rule applies.
#'
#' @format A data frame with one row per verification rule and 12 columns:
#' \describe{
#'   \item{verif_id}{Character. Unique identifier for the verification rule,
#'     named `ocv_<type><number>` (e.g., `ocv_pl1`). Matches the name of the
#'     function that implements the rule.}
#'   \item{type}{Character. Category of the verification (e.g.,
#'     `"plausibility"`).}
#'   \item{verification}{Character. Human-readable description of the rule.}
#'   \item{details}{Additional notes or details about the rule; empty (`NA`)
#'     when there are none.}
#'   \item{breast, crc, ge, larc_net, prostate, sarcomas, uitm, upgi}{
#'     Numeric (0/1) flags indicating whether the rule applies to that
#'     tumor group: Breast, CRC (colorectal), Gastroesophageal, LARC/NET,
#'     Prostate, Sarcomas, UITM, and Upper GI, respectively.}
#' }
#'
#' @source `data-raw/oncoclinical_verifications.R`
"oncoclinical_verifications"
