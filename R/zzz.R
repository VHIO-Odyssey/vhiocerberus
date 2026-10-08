.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.cerberus_banner(utils::packageVersion(pkgname)))
}

.cerberus_banner <- function(version = NULL) {
  # No Cerberus emoji exists; three wolves stand in for it,
  # centered over "CERBERUS" (each emoji takes 2 terminal columns)
  wolves <- "\U0001F43A\U0001F43A\U0001F43A"
  indent <- strrep(" ", nchar("VHIO") + floor((nchar("CERBERUS") - 6) / 2))
  title <- paste0(
    indent,
    wolves,
    "\n",
    cli::style_bold(cli::col_silver("VHIO"), cli::col_red("CERBERUS"))
  )
  footer <- cli::col_silver(paste0(
    "Automated Quality Checks for the VHIO Data Lake",
    if (!is.null(version)) paste0("  |  v", version) else ""
  ))
  paste0("\n", title, "\n", footer, "\n")
}
