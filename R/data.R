#' Oncoclinical data-verification rules
#'
#' A table of data-verification rules used to flag inconsistencies in the OncoClinical database, together with the tumor groups to which each rule applies.
#'
#' @format A data frame with one row per verification rule and 11 columns:
#' \describe{
#'   \item{verif_id}{Character. Unique identifier for the verification rule.}
#'   \item{verification}{Character. Human-readable description of the rule.}
#'   \item{details}{Character. Additional notes or details about the rule.}
#'   \item{Breast, CRC, Gastroesophageal, Gyn, LARC/NET, Prostate, UITM, Upper GI}{
#'     Integer (0/1) flags indicating whether the rule applies to that
#'     tumor group.}
#' }
#'
#' @source `data-raw/oncoclinical_verifications.R`
"oncoclinical_verifications"
