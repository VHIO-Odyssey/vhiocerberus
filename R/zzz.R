.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.cerberus_banner(utils::packageVersion(pkgname)))
}

.cerberus_banner <- function(version = NULL) {
  # Uppercase glyphs: all letters share the same height
  glyphs <- list(
    V = c("__   __", "\\ \\ / /", " \\ V / ", "  \\_/  "),
    H = c(" _  _ ", "| || |", "| __ |", "|_||_|"),
    I = c(" ___ ", "|_ _|", " | | ", "|___|"),
    O = c("  ___  ", " / _ \\ ", "| (_) |", " \\___/ "),
    C = c("  ___ ", " / __|", "| (__ ", " \\___|"),
    E = c(" ___ ", "| __|", "| _| ", "|___|"),
    R = c(" ___ ", "| _ \\", "|   /", "|_|_\\"),
    B = c(" ___ ", "| _ )", "| _ \\", "|___/"),
    U = c(" _   _ ", "| | | |", "| |_| |", " \\___/ "),
    S = c(" ___ ", "/ __|", "\\__ \\", "|___/")
  )

  word <- "VHIOCERBERUS"
  chars <- strsplit(word, "", fixed = TRUE)[[1]]

  rows <- vapply(
    seq_along(glyphs[[1]]),
    function(i) {
      paste0(
        vapply(chars, function(ch) glyphs[[ch]][i], character(1)),
        collapse = ""
      )
    },
    character(1)
  )

  # "vhio" in silver, "cerberus" in red
  split_at <- sum(nchar(vapply(
    chars[1:4],
    function(ch) glyphs[[ch]][1],
    character(1)
  )))
  banner <- vapply(
    rows,
    function(row) {
      paste0(
        cli::col_silver(substr(row, 1, split_at)),
        cli::col_red(substr(row, split_at + 1, nchar(row)))
      )
    },
    character(1)
  )

  footer <- cli::col_silver(paste0(
    "Automated Quality Checks for the VHIO Data Lake",
    if (!is.null(version)) paste0("  |  v", version) else ""
  ))

  # Three heads, centered over "cerberus"
  dog <- c(
    " /\\__/\\   /\\__/\\   /\\__/\\",
    "| o  o | | o  o | | o  o |",
    " \\ ^^ /   \\ ^^ /   \\ ^^ /"
  )
  cerberus_width <- nchar(rows[1]) - split_at
  indent <- strrep(
    " ",
    split_at + max(0, floor((cerberus_width - max(nchar(dog))) / 2))
  )
  dog <- cli::col_red(paste0(indent, dog))

  paste0("\n", paste0(c(dog, banner, footer), collapse = "\n"), "\n")
}
