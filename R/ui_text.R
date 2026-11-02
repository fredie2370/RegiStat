# Interface texts. They live in inst/i18n/ui.yml (one "en" and one "es" text
# per key) so that they can be corrected or translated without touching code.

.ui_cache <- new.env(parent = emptyenv())

#' All interface strings (internal)
#' @noRd
ui_strings <- function() {
  if (is.null(.ui_cache$strings)) {
    path <- system.file("i18n", "ui.yml", package = "RegiStat")
    if (!nzchar(path)) abort("The interface text file is missing.")
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    .ui_cache$strings <- yaml::yaml.load(paste(lines, collapse = "\n"))
  }
  .ui_cache$strings
}

#' One interface text in a language (internal)
#'
#' Extra arguments fill the `%s` / `%d` placeholders.
#' @noRd
ui_text <- function(key, lang = "en", ...) {
  entry <- ui_strings()[[key]]
  if (is.null(entry)) abort("Unknown interface text: %s", key)
  txt <- entry[[lang]]
  if (is.null(txt)) abort("No '%s' text for interface key: %s", lang, key)
  if (...length() > 0L) sprintf(txt, ...) else txt
}

#' Language options of the DataTables widget (internal)
#' @noRd
dt_language <- function(lang) {
  list(
    search = ui_text("dt_search", lang),
    lengthMenu = ui_text("dt_length", lang),
    info = ui_text("dt_info", lang),
    infoEmpty = ui_text("dt_info_empty", lang),
    emptyTable = ui_text("dt_empty", lang),
    zeroRecords = ui_text("dt_zero", lang),
    paginate = list(first = ui_text("dt_first", lang),
                    last = ui_text("dt_last", lang),
                    `next` = ui_text("dt_next", lang),
                    previous = ui_text("dt_previous", lang))
  )
}

#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Write a data frame as CSV that Excel opens correctly (internal)
#'
#' UTF-8 with a byte order mark so that accents survive, CRLF line ends, and
#' every field escaped. Written by hand because write.csv() replaces
#' non-ASCII characters with placeholders in non-UTF-8 locales.
#' @noRd
write_csv_excel <- function(df, file) {
  esc <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    needs <- grepl("[\",;\n\r]", x)
    x[needs] <- paste0("\"", gsub("\"", "\"\"", x[needs], fixed = TRUE), "\"")
    x
  }
  lines <- paste(esc(names(df)), collapse = ",")
  if (nrow(df) > 0L) {
    lines <- c(lines, do.call(paste, c(lapply(df, esc), sep = ",")))
  }
  bytes <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), bytes), file)
  invisible(file)
}
