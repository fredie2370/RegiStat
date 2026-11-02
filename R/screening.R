# Case finding ("cotejo"): checking which hospital file numbers appear in a
# reference list, and keeping a log of reviews.
#
# The file numbers live in their OWN SQLite file (screening.sqlite), never in
# the registry, which holds only pseudonymised study codes. An optional
# study code in a review is the only link between the two files.
#
# Nothing derived is stored: whether a review is "listed" is computed on
# demand against the chosen version of the reference list, so it can never be
# out of date. Loading a new list version is the "re-check".
#
# Like the registry, every change is appended to a hash-chained audit trail.

.screening_defaults <- list(strip_decimal_zero = TRUE, upper = TRUE)

#' Raise a classed screening error with a translated message (internal)
#' @noRd
scr_abort <- function(code, lang = "en", ...) {
  stop(structure(
    class = c("registat_screening_error", "error", "condition"),
    list(message = ui_text(code, lang, ...), call = NULL, code = code)))
}

#' Normalise file numbers so that equivalent spellings match (internal)
#'
#' Trims and collapses whitespace; removes a trailing `.0` that spreadsheets
#' add to numbers (`90913.0` becomes `90913`); optionally upper-cases.
#' Leading zeros are kept. Empty results become `NA`.
#' @noRd
normalize_file_no <- function(x, norm = .screening_defaults) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  if (isTRUE(norm$strip_decimal_zero)) x <- sub("^(.+)\\.0+$", "\\1", x)
  if (isTRUE(norm$upper)) x <- toupper(x)
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

#' Column number from a number or spreadsheet letters (internal)
#' @noRd
column_index <- function(column) {
  if (is.numeric(column)) return(as.integer(column))
  column <- toupper(trimws(as.character(column)))
  if (grepl("^[0-9]+$", column)) return(as.integer(column))
  if (!grepl("^[A-Z]{1,3}$", column)) {
    abort("Column must be a number or spreadsheet letters such as 'B'.")
  }
  letters_n <- utf8ToInt(column) - 64L
  as.integer(Reduce(function(acc, d) acc * 26L + d, letters_n, 0L))
}

#' Read the file numbers of a reference list
#'
#' Reads one column of a spreadsheet (`.xlsx`, `.xls`) or delimited text file
#' as raw cells, starting at a given row, and splits cells that hold several
#' file numbers (for example `"123/456"`). The result is not yet normalised;
#' [screening_load_list()] normalises and removes duplicates.
#'
#' @param path Path to the file.
#' @param sheet Sheet name or number (Excel only).
#' @param column Column number or spreadsheet letters, e.g. `2` or `"B"`.
#' @param first_row First row that holds a file number (skips titles and
#'   headers).
#' @param separator Text that separates several file numbers inside one cell;
#'   `NULL` or `""` for none.
#' @return A character vector of file numbers.
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("Diabetes list", "n;file", "1;90913.0", "2;123/456"), f)
#' read_reference_list(f, column = "B", first_row = 3)
#' @seealso [screening_load_list()]
#' @export
read_reference_list <- function(path, sheet = 1, column = 1, first_row = 1,
                                separator = "/") {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    abort("File not found: %s", paste(path, collapse = ", "))
  }
  first_row <- as.integer(first_row)
  if (length(first_row) != 1L || is.na(first_row) || first_row < 1L) {
    abort("'first_row' must be a positive whole number.")
  }
  col <- column_index(column)
  ext <- tolower(tools::file_ext(path))
  cells <- if (ext %in% c("xlsx", "xls")) {
    x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                            col_types = "text", .name_repair = "minimal",
                            skip = first_row - 1L)
    if (col > ncol(x)) abort("The file has only %d column(s).", ncol(x))
    x[[col]]
  } else if (ext %in% c("csv", "tsv", "txt")) {
    x <- read_delimited(path, header = FALSE)
    if (col > ncol(x)) abort("The file has only %d column(s).", ncol(x))
    if (first_row > nrow(x)) character() else x[[col]][first_row:nrow(x)]
  } else {
    abort("Unsupported file type '.%s'. Use .csv, .xlsx or .xls.", ext)
  }
  cells <- trimws(as.character(cells))
  cells <- cells[!is.na(cells) & nzchar(cells)]
  if (!is.null(separator) && nzchar(separator)) {
    cells <- unlist(strsplit(cells, separator, fixed = TRUE), use.names = FALSE)
  }
  cells <- trimws(cells)
  cells[nzchar(cells)]
}

# ---- store ------------------------------------------------------------------

#' Case-finding store
#'
#' Creates or opens the separate SQLite file that holds hospital file numbers,
#' the versions of the reference list and the log of reviews. It is a
#' different file from the registry on purpose: the registry contains only
#' pseudonymised study codes, while this file contains real file numbers and
#' must be protected accordingly (restricted folder, encrypted disk, never
#' shared or published).
#'
#' @details
#' A *reference list* is, for example, the list of file numbers of patients
#' with diabetes exported from the hospital system. Each upload is a new
#' *version*; old versions are kept. A *review* records that someone looked
#' for a file on a given date and whether it was found. Whether a reviewed
#' file number is *listed* is never stored: it is computed against a version
#' of the reference list when you ask, so it cannot be out of date.
#'
#' Every change is appended to a tamper-evident audit trail, exactly as for
#' the registry (see [registry_verify()] for what this does and does not
#' guarantee).
#'
#' @param path File path of the case-finding store (suggested name:
#'   `screening.sqlite`, next to the registry).
#' @param user Name of the person, saved in the audit trail.
#' @param normalize A list with `strip_decimal_zero` (turn `90913.0` into
#'   `90913`) and `upper` (ignore upper/lower case); both `TRUE` by default.
#'   Fixed when the store is created.
#' @param scr A store returned by `screening_create()` or `screening_open()`.
#' @return `screening_create()` and `screening_open()` return a
#'   `registat_screening` object.
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' scr <- screening_create(path, user = "demo")
#' screening_load_list(scr, c("90913", "123", "456"), user = "demo")
#' screening_add_review(scr, "123", "2024-05-02", found = TRUE, user = "demo")
#' screening_add_review(scr, "777", "2024-05-02", found = FALSE, user = "demo")
#' screening_reviews(scr)[, c("file_no", "found", "listed")]
#' screening_close(scr)
#' unlink(path)
#' @seealso [read_reference_list()], [screening_load_list()]
#' @export
screening_create <- function(path, user, normalize = list()) {
  check_user(user)
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    abort("'path' must be a single file path.")
  }
  if (file.exists(path)) {
    abort("File already exists: %s. Open it with screening_open().", path)
  }
  if (!dir.exists(dirname(path))) {
    abort("Directory does not exist: %s", dirname(path))
  }
  norm <- utils::modifyList(.screening_defaults, normalize)
  norm <- norm[names(.screening_defaults)]
  if (!all(vapply(norm, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1)))) {
    abort("'normalize' must contain TRUE or FALSE values.")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) {
    DBI::dbDisconnect(con)
    unlink(path)
  }, add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  scr <- structure(list(con = con, norm = norm, path = path),
                   class = "registat_screening")
  pkg_version <- tryCatch(as.character(utils::packageVersion("RegiStat")),
                          error = function(e) NA_character_)

  with_write(scr, {
    DBI::dbExecute(con, paste(
      "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"))
    DBI::dbExecute(con, paste(
      "CREATE TABLE reflists (version INTEGER PRIMARY KEY, loaded_at TEXT NOT NULL,",
      "source_name TEXT, config TEXT NOT NULL, n_items INTEGER NOT NULL,",
      "items_sha256 TEXT NOT NULL)"))
    DBI::dbExecute(con, paste(
      "CREATE TABLE reflist_items (version INTEGER NOT NULL, item TEXT NOT NULL,",
      "PRIMARY KEY (version, item))"))
    DBI::dbExecute(con, paste(
      "CREATE TABLE reviews (review_id INTEGER PRIMARY KEY, file_no TEXT NOT NULL,",
      "key TEXT NOT NULL, review_date TEXT NOT NULL, found INTEGER NOT NULL,",
      "study_code TEXT, note TEXT, created_at TEXT NOT NULL,",
      "updated_at TEXT NOT NULL, deleted INTEGER NOT NULL DEFAULT 0)"))
    DBI::dbExecute(con, paste(
      "CREATE TABLE audit (seq INTEGER PRIMARY KEY, ts TEXT NOT NULL,",
      "actor TEXT NOT NULL, action TEXT NOT NULL, record_id TEXT,",
      "changes TEXT NOT NULL, reason TEXT, prev_hash TEXT NOT NULL,",
      "hash TEXT NOT NULL)"))
    DBI::dbExecute(con, paste(
      "CREATE TRIGGER audit_no_update BEFORE UPDATE ON audit BEGIN",
      "SELECT RAISE(ABORT, 'audit log is append-only'); END"))
    DBI::dbExecute(con, paste(
      "CREATE TRIGGER audit_no_delete BEFORE DELETE ON audit BEGIN",
      "SELECT RAISE(ABORT, 'audit log is append-only'); END"))
    meta <- c(schema_version = .schema_version, kind = "screening",
              normalize = to_json(norm), created_at = now_utc(),
              package_version = pkg_version)
    meta <- meta[!is.na(meta)]
    DBI::dbExecute(con, "INSERT INTO meta (key, value) VALUES (?, ?)",
                   params = list(names(meta), unname(meta)))
    append_audit(con, user, "create", NA_character_,
                 to_json(list(normalize = norm)), NA_character_)
  })
  ok <- TRUE
  scr$path <- normalizePath(path)
  scr
}

#' @rdname screening_create
#' @export
screening_open <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    abort("Case-finding file not found: %s", paste(path, collapse = ", "))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  meta <- tryCatch(DBI::dbGetQuery(con, "SELECT key, value FROM meta"),
                   error = function(e) NULL)
  get <- function(k) meta$value[meta$key == k]
  if (is.null(meta) || !identical(get("kind"), "screening")) {
    abort("'%s' is not a RegiStat case-finding file.", path)
  }
  if (!identical(get("schema_version"), .schema_version)) {
    abort("Unsupported schema version: %s", get("schema_version"))
  }
  norm <- from_json(get("normalize"))
  ok <- TRUE
  structure(list(con = con, norm = norm, path = normalizePath(path)),
            class = "registat_screening")
}

#' @rdname screening_create
#' @export
screening_close <- function(scr) {
  if (inherits(scr, "registat_screening") && DBI::dbIsValid(scr$con)) {
    DBI::dbDisconnect(scr$con)
  }
  invisible(TRUE)
}

#' @export
print.registat_screening <- function(x, ...) {
  cat("<RegiStat case-finding store>\n")
  if (!DBI::dbIsValid(x$con)) {
    cat("  (closed)\n")
    return(invisible(x))
  }
  v <- latest_version(x$con)
  n <- DBI::dbGetQuery(x$con, "SELECT COUNT(*) AS n FROM reviews WHERE deleted = 0")$n
  cat(sprintf("  file: %s\n  reference list: %s\n  reviews: %d\n", x$path,
              if (is.na(v)) "none loaded" else sprintf("version %d", v), n))
  invisible(x)
}

#' @noRd
check_screening <- function(scr) {
  if (!inherits(scr, "registat_screening") || !DBI::dbIsValid(scr$con)) {
    abort("'scr' must be an open case-finding store.")
  }
  invisible(scr)
}

#' @noRd
latest_version <- function(con) {
  v <- DBI::dbGetQuery(con, "SELECT MAX(version) AS v FROM reflists")$v
  if (is.na(v)) NA_integer_ else as.integer(v)
}

#' @noRd
list_items <- function(con, version) {
  if (is.na(version)) return(character())
  DBI::dbGetQuery(con, "SELECT item FROM reflist_items WHERE version = ? ORDER BY item",
                  params = list(as.integer(version)))$item
}

# ---- reference list ---------------------------------------------------------

#' Load a reference list as a new version
#'
#' Normalises the file numbers (see [screening_create()]), removes duplicates
#' and stores them as the next version. Earlier versions are kept, so you can
#' compare them with [screening_changes()]. Loading a list identical to the
#' current one is refused.
#'
#' @param scr A case-finding store.
#' @param items Character vector of file numbers, for example from
#'   [read_reference_list()].
#' @param user Name of the person, saved in the audit trail.
#' @param source_name Optional name of the source file, saved with the
#'   version.
#' @param config Optional list describing how the list was read (sheet,
#'   column, first row, separator), saved with the version.
#' @param lang Language of messages: `"en"` or `"es"`.
#' @return The new version number, invisibly.
#' @export
screening_load_list <- function(scr, items, user, source_name = NA_character_,
                                config = list(), lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_screening(scr)
  check_user(user)
  items <- sort(unique(stats::na.omit(normalize_file_no(items, scr$norm))))
  if (length(items) == 0L) scr_abort("scr_err_list_empty", lang)
  sha <- sha256(paste(items, collapse = "\n"))
  source_name <- if (is.null(source_name) || is.na(source_name)) NA_character_ else
    as.character(source_name)

  version <- NULL
  with_write(scr, {
    cur <- latest_version(scr$con)
    if (!is.na(cur)) {
      prev <- DBI::dbGetQuery(scr$con,
        "SELECT items_sha256 FROM reflists WHERE version = ?",
        params = list(cur))$items_sha256
      if (identical(prev, sha)) scr_abort("scr_err_same_list", lang, cur)
    }
    version <- if (is.na(cur)) 1L else cur + 1L
    a <- append_audit(
      scr$con, user, "reflist_import", as.character(version),
      to_json(list(version = version, n_items = length(items),
                   items_sha256 = sha, source_name = source_name,
                   config = config)),
      NA_character_)
    DBI::dbExecute(scr$con, paste(
      "INSERT INTO reflists (version, loaded_at, source_name, config, n_items,",
      "items_sha256) VALUES (?, ?, ?, ?, ?, ?)"),
      params = list(version, a$ts, source_name, to_json(config),
                    length(items), sha))
    DBI::dbExecute(scr$con,
      "INSERT INTO reflist_items (version, item) VALUES (?, ?)",
      params = list(rep(version, length(items)), items))
  })
  invisible(version)
}

#' Versions of the reference list
#'
#' @param scr A case-finding store.
#' @return A data frame with `version`, `loaded_at` (UTC), `source_name` and
#'   `n_items`, newest last.
#' @export
screening_lists <- function(scr) {
  check_screening(scr)
  DBI::dbGetQuery(scr$con, paste(
    "SELECT version, loaded_at, source_name, n_items FROM reflists",
    "ORDER BY version"))
}

# ---- reviews ----------------------------------------------------------------

#' @noRd
scr_clean_date <- function(x, lang) {
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  x <- trimws(as.character(x))
  ok <- length(x) == 1L && !is.na(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) &&
    !is.na(as.Date(x, format = "%Y-%m-%d", optional = TRUE)) &&
    identical(format(as.Date(x, format = "%Y-%m-%d")), x)
  if (!ok) scr_abort("scr_err_date", lang, if (length(x) == 1L) x else "")
  x
}

#' @noRd
scr_clean_found <- function(x, lang) {
  if (length(x) == 1L && !is.na(x)) {
    if (is.logical(x)) return(as.integer(x))
    v <- tolower(trimws(as.character(x)))
    if (v %in% c("found", "1", "yes", "true")) return(1L)
    if (v %in% c("not_found", "notfound", "0", "no", "false")) return(0L)
  }
  scr_abort("scr_err_found", lang)
}

#' @noRd
scr_clean_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L])) return(NA_character_)
  x <- trimws(as.character(x[1L]))
  if (nzchar(x)) x else NA_character_
}

#' Record, change and remove reviews
#'
#' A review says that someone looked for a file number on a date and whether
#' the file was found. Reviews are corrected with a reason, and removed by
#' soft delete with a reason; both stay in the audit trail.
#'
#' The same file number can be reviewed on different dates (for example every
#' month) but not twice on the same date.
#'
#' @param scr A case-finding store.
#' @param file_no The hospital file number, as written.
#' @param review_date Date of the review (`Date` or `"YYYY-MM-DD"`).
#' @param found `TRUE` if the file was found, `FALSE` if not.
#' @param user Name of the person, saved in the audit trail.
#' @param study_code Optional pseudonymised study code of the registry
#'   record, to link the two files.
#' @param note Optional free text. Do not put names or other identifiers.
#' @param review_id Number of the review, as returned by
#'   `screening_add_review()` or listed by [screening_reviews()].
#' @param changes Named list with any of `review_date`, `found`,
#'   `study_code` and `note`. The file number cannot be changed: delete the
#'   review and add a new one.
#' @param reason Why the review is changed or deleted (required).
#' @param lang Language of messages: `"en"` or `"es"`.
#' @return `screening_add_review()` returns the new review number; the others
#'   return it invisibly.
#' @export
screening_add_review <- function(scr, file_no, review_date, found, user,
                                 study_code = NULL, note = NULL,
                                 lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_screening(scr)
  check_user(user)
  raw_no <- scr_clean_text(file_no)
  key <- normalize_file_no(raw_no, scr$norm)
  if (is.na(key)) scr_abort("scr_err_file_empty", lang)
  date <- scr_clean_date(review_date, lang)
  found <- scr_clean_found(found, lang)
  study_code <- scr_clean_text(study_code)
  note <- scr_clean_text(note)

  id <- NULL
  with_write(scr, {
    dup <- DBI::dbGetQuery(scr$con, paste(
      "SELECT review_id FROM reviews WHERE key = ? AND review_date = ?",
      "AND deleted = 0"), params = list(key, date))
    if (nrow(dup) > 0L) scr_abort("scr_err_duplicate", lang, raw_no, date)
    mx <- DBI::dbGetQuery(scr$con, "SELECT MAX(review_id) AS m FROM reviews")$m
    id <- if (is.na(mx)) 1L else as.integer(mx) + 1L
    a <- append_audit(
      scr$con, user, "review_add", as.character(id),
      to_json(list(file_no = raw_no, key = key, review_date = date,
                   found = found, study_code = study_code, note = note)),
      NA_character_)
    DBI::dbExecute(scr$con, paste(
      "INSERT INTO reviews (review_id, file_no, key, review_date, found,",
      "study_code, note, created_at, updated_at, deleted)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)"),
      params = list(id, raw_no, key, date, found, study_code, note, a$ts, a$ts))
  })
  id
}

#' @noRd
fetch_review <- function(con, id) {
  r <- DBI::dbGetQuery(con, "SELECT * FROM reviews WHERE review_id = ?",
                       params = list(as.integer(id)))
  if (nrow(r) == 0L) NULL else r
}

#' @rdname screening_add_review
#' @export
screening_update_review <- function(scr, review_id, changes, user, reason,
                                    lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_screening(scr)
  check_user(user)
  if (!nzchar(trimws(as.character(reason %||% ""))) || is.na(reason)) {
    scr_abort("reason_required", lang)
  }
  allowed <- c("review_date", "found", "study_code", "note")
  if (!is.list(changes) || length(changes) == 0L || is.null(names(changes)) ||
      !all(names(changes) %in% allowed)) {
    abort("'changes' must be a named list using: %s", paste(allowed, collapse = ", "))
  }
  review_id <- as.integer(review_id)

  with_write(scr, {
    row <- fetch_review(scr$con, review_id)
    if (is.null(row)) scr_abort("scr_err_review_missing", lang, review_id)
    if (row$deleted == 1L) scr_abort("scr_err_review_deleted", lang, review_id)
    new <- list(review_date = row$review_date, found = row$found,
                study_code = row$study_code, note = row$note)
    if ("review_date" %in% names(changes)) {
      new$review_date <- scr_clean_date(changes$review_date, lang)
    }
    if ("found" %in% names(changes)) new$found <- scr_clean_found(changes$found, lang)
    if ("study_code" %in% names(changes)) new$study_code <- scr_clean_text(changes$study_code)
    if ("note" %in% names(changes)) new$note <- scr_clean_text(changes$note)

    differs <- names(new)[vapply(names(new), function(nm) {
      a <- row[[nm]]
      b <- new[[nm]]
      xor(is.na(a), is.na(b)) || (!is.na(a) && !is.na(b) && a != b)
    }, logical(1))]
    if (length(differs) == 0L) scr_abort("scr_err_no_changes", lang)
    if ("review_date" %in% differs) {
      dup <- DBI::dbGetQuery(scr$con, paste(
        "SELECT review_id FROM reviews WHERE key = ? AND review_date = ?",
        "AND deleted = 0 AND review_id <> ?"),
        params = list(row$key, new$review_date, review_id))
      if (nrow(dup) > 0L) scr_abort("scr_err_duplicate", lang, row$file_no,
                                    new$review_date)
    }
    diff_list <- stats::setNames(
      lapply(differs, function(nm) list(old = row[[nm]], new = new[[nm]])),
      differs)
    a <- append_audit(scr$con, user, "review_update", as.character(review_id),
                      to_json(diff_list), reason)
    DBI::dbExecute(scr$con, paste(
      "UPDATE reviews SET review_date = ?, found = ?, study_code = ?, note = ?,",
      "updated_at = ? WHERE review_id = ?"),
      params = list(new$review_date, new$found, new$study_code, new$note,
                    a$ts, review_id))
  })
  invisible(review_id)
}

#' @rdname screening_add_review
#' @export
screening_delete_review <- function(scr, review_id, user, reason,
                                    lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_screening(scr)
  check_user(user)
  if (!nzchar(trimws(as.character(reason %||% ""))) || is.na(reason)) {
    scr_abort("reason_required", lang)
  }
  review_id <- as.integer(review_id)
  with_write(scr, {
    row <- fetch_review(scr$con, review_id)
    if (is.null(row)) scr_abort("scr_err_review_missing", lang, review_id)
    if (row$deleted == 1L) scr_abort("scr_err_review_deleted", lang, review_id)
    a <- append_audit(scr$con, user, "review_delete", as.character(review_id),
                      "{}", reason)
    DBI::dbExecute(scr$con,
      "UPDATE reviews SET deleted = 1, updated_at = ? WHERE review_id = ?",
      params = list(a$ts, review_id))
  })
  invisible(review_id)
}

#' Reviews with their derived status
#'
#' Returns the reviews and, for each, whether its file number is in the
#' reference list. The status is computed now, against `version` (the newest
#' by default); it is never stored.
#'
#' @param scr A case-finding store.
#' @param version Version of the reference list to compare with; the newest
#'   when `NULL`.
#' @param include_deleted If `TRUE`, deleted reviews are included, flagged in
#'   `.deleted`.
#' @return A data frame with `review_id`, `file_no`, `review_date` (`Date`),
#'   `found` (logical), `listed` (logical; `NA` when no list is loaded),
#'   `study_code` and `note`.
#' @export
screening_reviews <- function(scr, version = NULL, include_deleted = FALSE) {
  check_screening(scr)
  ver <- if (is.null(version)) latest_version(scr$con) else as.integer(version)
  q <- paste("SELECT review_id, file_no, key, review_date, found, study_code,",
             "note, deleted FROM reviews")
  if (!isTRUE(include_deleted)) q <- paste(q, "WHERE deleted = 0")
  r <- DBI::dbGetQuery(scr$con, paste(q, "ORDER BY review_date, review_id"))
  listed <- if (is.na(ver)) rep(NA, nrow(r)) else r$key %in% list_items(scr$con, ver)
  out <- data.frame(
    review_id = as.integer(r$review_id), file_no = r$file_no,
    review_date = as.Date(r$review_date, format = "%Y-%m-%d"),
    found = r$found == 1L, listed = as.logical(listed),
    study_code = r$study_code, note = r$note, stringsAsFactors = FALSE)
  if (isTRUE(include_deleted)) out$.deleted <- r$deleted == 1L
  out
}

#' File numbers on the list that have not been reviewed
#'
#' @param scr A case-finding store.
#' @param version Version of the reference list; the newest when `NULL`.
#' @return A character vector of normalised file numbers.
#' @export
screening_pending <- function(scr, version = NULL) {
  check_screening(scr)
  ver <- if (is.null(version)) latest_version(scr$con) else as.integer(version)
  items <- list_items(scr$con, ver)
  keys <- DBI::dbGetQuery(scr$con,
                          "SELECT DISTINCT key FROM reviews WHERE deleted = 0")$key
  setdiff(items, keys)
}

#' Summary of reviews by month
#'
#' Counts reviews by month, whether the file was found and whether it is
#' listed, using the newest reference list.
#'
#' @param scr A case-finding store.
#' @return A data frame with `month` (`"YYYY-MM"`), `found`, `listed` and
#'   `n`.
#' @export
screening_summary <- function(scr) {
  r <- screening_reviews(scr)
  if (nrow(r) == 0L) {
    return(data.frame(month = character(), found = logical(),
                      listed = logical(), n = integer()))
  }
  r$month <- format(r$review_date, "%Y-%m")
  tab <- stats::aggregate(review_id ~ month + found + listed, data = r,
                          FUN = length, na.action = stats::na.pass)
  names(tab)[4L] <- "n"
  tab <- tab[order(tab$month, !tab$found, !tab$listed), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

#' Changes between two versions of the reference list
#'
#' @param scr A case-finding store.
#' @param from,to Version numbers; by default the two newest.
#' @param lang Language of messages: `"en"` or `"es"`.
#' @return A list with `from`, `to`, `added` and `removed` (file numbers) and
#'   `reviews`, a data frame of the reviews whose listed status differs
#'   between the two versions.
#' @export
screening_changes <- function(scr, from = NULL, to = NULL, lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_screening(scr)
  v <- screening_lists(scr)$version
  if (is.null(from) || is.null(to)) {
    if (length(v) < 2L) scr_abort("scr_err_need_two", lang)
    to <- v[length(v)]
    from <- v[length(v) - 1L]
  }
  if (!all(c(from, to) %in% v)) scr_abort("scr_err_version_missing", lang)
  a <- list_items(scr$con, from)
  b <- list_items(scr$con, to)
  r_from <- screening_reviews(scr, version = from)
  r_to <- screening_reviews(scr, version = to)
  changed <- r_from$listed != r_to$listed
  rev <- r_to[which(changed), c("review_id", "file_no", "review_date"), drop = FALSE]
  rev$listed_from <- r_from$listed[which(changed)]
  rev$listed_to <- r_to$listed[which(changed)]
  list(from = from, to = to, added = setdiff(b, a), removed = setdiff(a, b),
       reviews = rev)
}

# ---- audit ------------------------------------------------------------------

#' Audit trail and integrity check of the case-finding store
#'
#' `screening_audit()` returns the audit trail. `screening_verify()` checks
#' the hash chain and replays the trail to compare it with the reviews and the
#' reference-list versions; see [registry_verify()] for the guarantees and
#' limits.
#'
#' @param scr A case-finding store.
#' @param expected_head Optional head hash saved earlier.
#' @return `screening_audit()` a data frame; `screening_verify()` a
#'   `registat_verification` object.
#' @export
screening_audit <- function(scr) {
  check_screening(scr)
  DBI::dbGetQuery(scr$con, paste(
    "SELECT seq, ts AS timestamp, actor AS user, action, record_id, reason,",
    "changes, hash FROM audit ORDER BY seq"))
}

#' @rdname screening_audit
#' @export
screening_verify <- function(scr, expected_head = NULL) {
  check_screening(scr)
  a <- DBI::dbGetQuery(scr$con, paste(
    "SELECT seq, ts, actor, action, record_id, changes, reason, prev_hash, hash",
    "FROM audit ORDER BY seq"))
  n <- nrow(a)
  problems <- c(chain_problems(a), screening_replay_problems(scr, a))
  head <- if (n > 0L) a$hash[n] else .genesis_hash
  if (!is.null(expected_head) && !identical(as.character(expected_head), head)) {
    problems <- c(problems, paste(
      "The head of the audit chain does not match the expected value:",
      "entries may have been removed or rewritten."))
  }
  structure(list(ok = length(problems) == 0L, n_entries = n, head = head,
                 problems = problems), class = "registat_verification")
}

#' @noRd
screening_replay_problems <- function(scr, a) {
  problems <- character()
  fields <- c("file_no", "key", "review_date", "found", "study_code", "note")
  reviews <- list()
  versions <- list()
  for (i in seq_len(nrow(a))) {
    act <- a$action[i]
    id <- a$record_id[i]
    ok <- tryCatch({
      ch <- from_json(a$changes[i])
      if (identical(act, "review_add")) {
        reviews[[id]] <- list(
          values = lapply(stats::setNames(fields, fields), function(f) {
            x <- ch[[f]]
            if (is.null(x)) NA_character_ else as.character(x)
          }), deleted = FALSE)
      } else if (identical(act, "review_update")) {
        for (nm in names(ch)) {
          nv <- ch[[nm]][["new"]]
          reviews[[id]]$values[[nm]] <- if (is.null(nv)) NA_character_ else as.character(nv)
        }
      } else if (identical(act, "review_delete")) {
        reviews[[id]]$deleted <- TRUE
      } else if (identical(act, "reflist_import")) {
        versions[[id]] <- list(n = ch$n_items, sha = ch$items_sha256)
      }
      TRUE
    }, error = function(e) FALSE)
    if (!ok) problems <- c(problems, sprintf(
      "Audit entry %d could not be replayed.", a$seq[i]))
  }

  rv <- DBI::dbGetQuery(scr$con, "SELECT * FROM reviews")
  for (id in setdiff(as.character(rv$review_id), names(reviews))) {
    problems <- c(problems, sprintf(
      "Review %s exists in the table but not in the audit trail.", id))
  }
  for (id in setdiff(names(reviews), as.character(rv$review_id))) {
    problems <- c(problems, sprintf(
      "Review %s is in the audit trail but missing from the table.", id))
  }
  for (j in seq_len(nrow(rv))) {
    id <- as.character(rv$review_id[j])
    if (!(id %in% names(reviews))) next
    exp <- reviews[[id]]
    same <- vapply(fields, function(f) {
      x <- rv[[f]][j]
      y <- exp$values[[f]]
      if (identical(f, "found")) x <- as.character(x)
      (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(as.character(x), as.character(y)))
    }, logical(1))
    if (!all(same) || (rv$deleted[j] == 1L) != isTRUE(exp$deleted)) {
      problems <- c(problems, sprintf(
        "Review %s does not match its audit trail (edited outside RegiStat?).", id))
    }
  }

  rl <- DBI::dbGetQuery(scr$con, "SELECT version, n_items, items_sha256 FROM reflists")
  for (id in setdiff(as.character(rl$version), names(versions))) {
    problems <- c(problems, sprintf(
      "Reference list version %s exists in the table but not in the audit trail.", id))
  }
  for (id in setdiff(names(versions), as.character(rl$version))) {
    problems <- c(problems, sprintf(
      "Reference list version %s is in the audit trail but missing from the table.", id))
  }
  for (j in seq_len(nrow(rl))) {
    id <- as.character(rl$version[j])
    items <- list_items(scr$con, rl$version[j])
    sha <- sha256(paste(items, collapse = "\n"))
    exp <- versions[[id]]
    if (!identical(sha, rl$items_sha256[j]) ||
        (!is.null(exp) && (!identical(sha, exp$sha) ||
                           !identical(as.integer(length(items)), as.integer(exp$n))))) {
      problems <- c(problems, sprintf(
        "Reference list version %s does not match its audit trail (edited outside RegiStat?).", id))
    }
  }
  problems
}
