# Helpers to bring spreadsheet data into a registry: reading CSV/Excel files as
# text, matching columns to dictionary variables, and converting the usual
# spreadsheet formats (Excel dates, dd/mm/yyyy, decimal comma, value labels)
# into the forms the validation engine expects. Nothing here writes to a
# registry; see registry_check() and registry_import() for that.

.accent_from <- intToUtf8(c(0xe1, 0xe9, 0xed, 0xf3, 0xfa, 0xfc, 0xf1,
                              0xc1, 0xc9, 0xcd, 0xd3, 0xda, 0xdc, 0xd1))
.accent_to <- "aeiouunAEIOUUN"

#' Lower-case text without accents or punctuation, for matching (internal)
#' @noRd
simplify_text <- function(x) {
  x <- chartr(.accent_from, .accent_to, enc2utf8(as.character(x)))
  gsub("[^a-z0-9]", "", tolower(x))
}

#' Read a CSV or Excel file with every value as text
#'
#' Reads `.csv`, `.tsv`, `.txt`, `.xlsx` or `.xls` files without guessing
#' types, so identifiers such as `00123` and values such as `85.0` reach the
#' validation engine exactly as written. For CSV files the delimiter (comma,
#' semicolon, tab or pipe) and the encoding (UTF-8, with or without a byte
#' order mark, or Latin-1) are detected. Excel dates are read as serial
#' numbers; convert them with [convert_import_values()].
#'
#' Whitespace is trimmed, empty cells become `NA`, and columns or rows that are
#' completely empty are dropped.
#'
#' @param path Path to the file.
#' @param sheet Sheet name or number for Excel files (default: the first).
#' @return A data frame of character columns.
#' @seealso [table_sheets()], [suggest_mapping()], [convert_import_values()]
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("id;age", "A1;60", "A2;7"), f)
#' read_table_file(f)
#' @export
read_table_file <- function(path, sheet = NULL) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    abort("File not found: %s", paste(path, collapse = ", "))
  }
  ext <- tolower(tools::file_ext(path))
  df <- if (ext %in% c("xlsx", "xls")) {
    x <- readxl::read_excel(path, sheet = if (is.null(sheet)) 1L else sheet,
                            col_types = "text", .name_repair = "minimal")
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("csv", "tsv", "txt")) {
    read_delimited(path)
  } else {
    abort("Unsupported file type '.%s'. Use .csv, .xlsx or .xls.", ext)
  }
  clean_table(df)
}

#' Sheet names of an Excel file
#'
#' @param path Path to an `.xlsx` or `.xls` file.
#' @return A character vector of sheet names, or `NULL` for other file types.
#' @export
table_sheets <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(path) else NULL
}

#' @noRd
read_delimited <- function(path, header = TRUE) {
  raw <- readBin(path, "raw", file.info(path)$size)
  if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    raw <- raw[-(1:3)]
  }
  if (length(raw) == 0L) abort("The file is empty.")
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) {
    Encoding(txt) <- "latin1"
    txt <- enc2utf8(txt)
  }
  txt <- gsub("\r\n?", "\n", txt)
  all_lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  all_lines <- all_lines[nzchar(trimws(all_lines))]
  # With a header the first line decides. Without one (raw cells, where the
  # first lines may be titles without separators) look at the first lines.
  first <- if (header) all_lines[1L] else utils::head(all_lines, 20L)
  seps <- c(";", ",", "\t", "|")
  counts <- vapply(seps, function(s) {
    sum(lengths(regmatches(first, gregexpr(s, first, fixed = TRUE))))
  }, integer(1))
  sep <- if (max(counts) == 0L) "," else seps[which.max(counts)]
  tryCatch({
    if (header) {
      utils::read.table(text = txt, sep = sep, header = TRUE, quote = "\"",
                        colClasses = "character", check.names = FALSE,
                        comment.char = "", strip.white = TRUE, fill = TRUE,
                        stringsAsFactors = FALSE, blank.lines.skip = TRUE)
    } else {
      # Raw cells: keep blank lines so that row numbers match the file.
      k <- max(utils::count.fields(textConnection(txt), sep = sep, quote = "\"",
                                   blank.lines.skip = FALSE), na.rm = TRUE)
      utils::read.table(text = txt, sep = sep, header = FALSE, quote = "\"",
                        col.names = paste0("V", seq_len(k)),
                        colClasses = "character", comment.char = "",
                        strip.white = TRUE, fill = TRUE,
                        stringsAsFactors = FALSE, blank.lines.skip = FALSE)
    }
  },
    error = function(e) abort("The file could not be read as a table: %s",
                              conditionMessage(e))
  )
}

#' @noRd
clean_table <- function(df) {
  df[] <- lapply(df, function(x) {
    x <- trimws(as.character(x))
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    x
  })
  nm <- trimws(names(df))
  nm[is.na(nm)] <- ""
  names(df) <- nm
  empty_col <- vapply(df, function(x) all(is.na(x)), logical(1)) & !nzchar(nm)
  df <- df[, !empty_col, drop = FALSE]
  if (nrow(df) > 0L && ncol(df) > 0L) {
    # Column by column: is.na() on a data frame would try to translate
    # non-ASCII column names to the native encoding (warns in C locales).
    all_na <- Reduce(`&`, lapply(df, is.na))
    df <- df[!all_na, , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

#' Suggest which file column holds each dictionary variable
#'
#' Compares the column names of a file with the variable names and the English
#' and Spanish labels of the dictionary, ignoring case, accents and
#' punctuation. Each column is used at most once.
#'
#' @param columns Character vector of column names from the file.
#' @param dict A `registat_dictionary`.
#' @return A character vector named by variable, with the matching column name
#'   or `NA` when none was found.
#' @examples
#' dict <- load_template("t2d")
#' suggest_mapping(c("Edad", "Peso corporal", "HBA1C PCT", "otra"), dict)[c(
#'   "age_years", "weight_kg", "hba1c_pct")]
#' @export
suggest_mapping <- function(columns, dict) {
  cols_key <- simplify_text(columns)
  used <- logical(length(columns))
  out <- stats::setNames(rep(NA_character_, length(dict$variables)),
                         variable_names(dict))
  for (v in dict$variables) {
    keys <- unique(simplify_text(c(v$name, v$label$en, v$label$es)))
    hit <- which(cols_key %in% keys & !used)
    if (length(hit) > 0L) {
      out[[v$name]] <- columns[hit[1L]]
      used[hit[1L]] <- TRUE
    }
  }
  out
}

#' Rename file columns to dictionary variables
#'
#' @param data A data frame, for example from [read_table_file()].
#' @param mapping A named character vector as returned by
#'   [suggest_mapping()]; entries that are `NA` or empty are skipped.
#' @return A data frame with only the mapped columns, named by variable.
#' @export
apply_mapping <- function(data, mapping) {
  mapping <- mapping[!is.na(mapping) & nzchar(mapping)]
  bad <- setdiff(unname(mapping), names(data))
  if (length(bad) > 0L) {
    abort("Column(s) not found in the data: %s", paste(bad, collapse = ", "))
  }
  if (anyDuplicated(unname(mapping))) {
    abort("A file column cannot be assigned to two variables.")
  }
  out <- data[, unname(mapping), drop = FALSE]
  names(out) <- names(mapping)
  out
}

#' Convert spreadsheet values to the forms the dictionary expects
#'
#' Applied to a character data frame whose columns are named by dictionary
#' variables:
#'
#' * **Dates:** Excel serial numbers, ISO dates (a trailing zero time is
#'   dropped) and, depending on `date_format`, `dd/mm/yyyy` or `mm/dd/yyyy`
#'   (separators `/`, `-` or `.`) become `YYYY-MM-DD`. Ambiguous formats are
#'   never guessed: choose the format that your file uses. Values that do not
#'   parse are left unchanged so that validation reports them.
#' * **Numbers:** with `decimal_mark = ","`, `85,3` becomes `85.3`.
#' * **Categories:** a value is replaced by the dictionary code when it equals
#'   the code or the English or Spanish label, ignoring case and accents
#'   (the Spanish word for yes, with its accent, becomes `"yes"`).
#'
#' Everything else is left as it is.
#'
#' @param data A character data frame named by variable.
#' @param dict A `registat_dictionary`.
#' @param date_format `"iso"`, `"dmy"` (day first) or `"mdy"` (month first).
#' @param decimal_mark `"."` or `","`.
#' @return The converted data frame.
#' @examples
#' dict <- load_template("t2d")
#' d <- data.frame(visit_date = c("01/05/2024", "45413"),
#'                 weight_kg = c("82,5", "70"),
#'                 sex_at_birth = c("Mujer", "male"))
#' convert_import_values(d, dict, date_format = "dmy", decimal_mark = ",")
#' @export
convert_import_values <- function(data, dict, date_format = c("iso", "dmy", "mdy"),
                                  decimal_mark = c(".", ",")) {
  date_format <- match.arg(date_format)
  decimal_mark <- match.arg(decimal_mark)
  for (v in dict$variables) {
    nm <- v$name
    if (!(nm %in% names(data))) next
    x <- as.character(data[[nm]])
    x <- switch(
      v$type,
      date = parse_date_text(x, date_format),
      integer = ,
      numeric = if (decimal_mark == ",") {
        ifelse(grepl("^-?[0-9]+,[0-9]+$", x), sub(",", ".", x, fixed = TRUE), x)
      } else x,
      categorical = map_choice_values(x, v),
      x
    )
    data[[nm]] <- x
  }
  data
}

#' @noRd
parse_date_text <- function(x, fmt) {
  out <- x
  ok <- !is.na(x)
  ser <- ok & grepl("^[0-9]{5}(\\.[0-9]+)?$", x)
  if (any(ser)) {
    out[ser] <- format(as.Date(floor(as.numeric(x[ser])),
                               origin = "1899-12-30"), "%Y-%m-%d")
  }
  iso <- ok & !ser &
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T]00:00:00(\\.0+)?)?$", x)
  out[iso] <- substr(x[iso], 1L, 10L)

  if (fmt != "iso") {
    sl <- which(ok & !ser & !iso &
                  grepl("^[0-9]{1,2}[/.-][0-9]{1,2}[/.-][0-9]{4}$", x))
    if (length(sl) > 0L) {
      parts <- strsplit(x[sl], "[/.-]")
      a <- as.integer(vapply(parts, `[`, "", 1L))
      b <- as.integer(vapply(parts, `[`, "", 2L))
      year <- vapply(parts, `[`, "", 3L)
      day <- if (fmt == "dmy") a else b
      month <- if (fmt == "dmy") b else a
      cand <- sprintf("%s-%02d-%02d", year, month, day)
      valid <- !is.na(as.Date(cand, format = "%Y-%m-%d"))
      out[sl[valid]] <- cand[valid]
    }
  }
  out
}

#' @noRd
map_choice_values <- function(x, v) {
  keys <- character()
  vals <- character()
  for (ch in v$choices) {
    k <- unique(simplify_text(c(ch$value, ch$label$en, ch$label$es)))
    keys <- c(keys, k)
    vals <- c(vals, rep(ch$value, length(k)))
  }
  ambiguous <- unique(keys[duplicated(keys) & !duplicated(paste(keys, vals))])
  keep <- !(keys %in% ambiguous) & !duplicated(keys)
  lookup <- stats::setNames(vals[keep], keys[keep])
  key <- simplify_text(x)
  hit <- !is.na(x) & key %in% names(lookup)
  x[hit] <- unname(lookup[key[hit]])
  x
}
