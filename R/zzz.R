.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.cerberus_banner(utils::packageVersion(pkgname)))
}

.cerberus_banner <- function(version = NULL) {
  title <- paste0(
    cli::style_bold(cli::col_silver("VHIO"), cli::col_red("CERBERUS")),
    if (!is.null(version)) cli::col_silver(paste0("  v", version)) else ""
  )
  footer <- cli::col_silver("Automated Quality Checks for the VHIO Data Lake")
  paste0("\n", title, "\n", footer, "\n")
}
