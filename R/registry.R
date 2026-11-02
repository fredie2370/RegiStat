#' Create, open and close a registry
#'
#' A registry is a single local SQLite file that holds the records of one
#' study, a copy of the data dictionary it was created with, and an
#' append-only audit trail. Nothing is sent over the network.
#'
#' Records are stored as text and converted back to the types declared in
#' the dictionary by [registry_data()]. Every change goes through the
#' validation engine and is written to the audit trail in the same
#' transaction: either both succeed or neither does.
#'
#' `registry_create()` refuses to touch an existing file, so it can never
#' overwrite a registry that already holds data.
#'
#' @param path Path of the registry file. For `registry_create()` it must not
#'   exist yet.
#' @param dict A `registat_dictionary`, from [read_dictionary()] or
#'   [load_template()].
#' @param user Name of the person making the change. It is recorded in the
#'   audit trail but not authenticated.
#' @param reg A registry returned by `registry_create()` or `registry_open()`.
#' @return `registry_create()` and `registry_open()` return a
#'   `registat_registry` object. `registry_close()` returns `TRUE` invisibly.
#' @seealso [registry_add()], [registry_verify()]
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' reg <- registry_create(path, load_template("t2d"), user = "demo")
#' registry_close(reg)
#' reg <- registry_open(path)
#' reg
#' registry_close(reg)
#' unlink(path)
#' @export
registry_create <- function(path, dict, user) {
  check_user(user)
  if (!inherits(dict, "registat_dictionary")) {
    abort("'dict' must come from read_dictionary() or load_template().")
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    abort("'path' must be a single file path.")
  }
  if (file.exists(path)) {
    abort("File already exists: %s. Open it with registry_open().", path)
  }
  if (!dir.exists(dirname(path))) {
    abort("Directory does not exist: %s", dirname(path))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) {
    DBI::dbDisconnect(con)
    unlink(path)
  }, add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

  reg <- structure(list(con = con, dict = dict, path = path),
                   class = "registat_registry")
  dict_json <- to_json(unclass(dict))
  dict_hash <- sha256(dict_json)
  pkg_version <- tryCatch(as.character(utils::packageVersion("RegiStat")),
                          error = function(e) NA_character_)

  with_write(reg, {
    DBI::dbExecute(con, paste(
      "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"))
    DBI::dbExecute(con, paste(
      "CREATE TABLE records (record_id TEXT PRIMARY KEY, data TEXT NOT NULL,",
      "created_seq INTEGER NOT NULL, created_at TEXT NOT NULL,",
      "updated_at TEXT NOT NULL, version INTEGER NOT NULL,",
      "deleted INTEGER NOT NULL DEFAULT 0)"))
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

    meta <- c(schema_version = .schema_version,
              dictionary = dict_json,
              dictionary_sha256 = dict_hash,
              study_id = if (is.null(dict$study$id)) "" else dict$study$id,
              created_at = now_utc(),
              package_version = pkg_version)
    meta <- meta[!is.na(meta)]
    DBI::dbExecute(con, "INSERT INTO meta (key, value) VALUES (?, ?)",
                   params = list(names(meta), unname(meta)))
    append_audit(con, user, "create", NA_character_,
                 to_json(list(dictionary_sha256 = dict_hash)), NA_character_)
  })

  ok <- TRUE
  reg$path <- normalizePath(path)
  reg
}

#' @rdname registry_create
#' @export
registry_open <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    abort("Registry file not found: %s", paste(path, collapse = ", "))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

  meta <- tryCatch(DBI::dbGetQuery(con, "SELECT key, value FROM meta"),
                   error = function(e) NULL)
  get <- function(k) meta$value[meta$key == k]
  if (is.null(meta) || length(get("schema_version")) != 1L) {
    abort("'%s' is not a RegiStat registry.", path)
  }
  if (!identical(get("schema_version"), .schema_version)) {
    abort("Unsupported registry schema version: %s", get("schema_version"))
  }
  dict_json <- get("dictionary")
  if (!identical(sha256(dict_json), get("dictionary_sha256"))) {
    warning("The stored dictionary does not match its recorded hash.",
            call. = FALSE)
  }
  dict <- validate_dictionary(from_json(dict_json))

  ok <- TRUE
  structure(list(con = con, dict = dict, path = normalizePath(path)),
            class = "registat_registry")
}

#' @rdname registry_create
#' @export
registry_close <- function(reg) {
  if (inherits(reg, "registat_registry") && DBI::dbIsValid(reg$con)) {
    DBI::dbDisconnect(reg$con)
  }
  invisible(TRUE)
}

#' @export
print.registat_registry <- function(x, ...) {
  cat("<RegiStat registry>\n")
  cat("File:  ", x$path, "\n", sep = "")
  if (!DBI::dbIsValid(x$con)) {
    cat("Status: closed\n")
    return(invisible(x))
  }
  n_active <- DBI::dbGetQuery(
    x$con, "SELECT COUNT(*) AS n FROM records WHERE deleted = 0")$n
  n_deleted <- DBI::dbGetQuery(
    x$con, "SELECT COUNT(*) AS n FROM records WHERE deleted = 1")$n
  n_audit <- DBI::dbGetQuery(x$con, "SELECT COUNT(*) AS n FROM audit")$n
  cat("Study: ", if (is.null(x$dict$study$id)) "-" else x$dict$study$id,
      "\n", sep = "")
  cat(sprintf("Records: %d active, %d deleted\nAudit entries: %d\n",
              n_active, n_deleted, n_audit))
  invisible(x)
}

#' Add, update, delete and import records
#'
#' All changes are validated against the dictionary and logged in the audit
#' trail in one transaction. If validation fails, nothing is written and a
#' condition of class `registat_validation_error` is raised; its `issues`
#' element is the data frame returned by [validate_data()].
#'
#' * `registry_add()` adds one record, given as a named list or a one-row
#'   data frame. Variables left out are stored as missing. Identifiers are
#'   unique, and an identifier that belonged to a deleted record cannot be
#'   reused.
#' * `registry_update()` changes some variables of an existing record. A
#'   `reason` is mandatory, and the audit trail keeps the old and new values.
#'   The identifier cannot be changed.
#' * `registry_delete()` is a *soft* delete: the record is hidden from
#'   [registry_data()] but stays in the database and in the audit trail. A
#'   `reason` is mandatory.
#' * `registry_import()` adds many records from a data frame. It is
#'   all-or-nothing: if any row is invalid, no row is imported.
#'
#' @param reg A registry from [registry_open()] or [registry_create()].
#' @param record A named list or one-row data frame with the variables of the
#'   dictionary.
#' @param id Identifier of the record to change.
#' @param changes A named list with the new values, for example
#'   `list(age_years = 61)`. Use `NA` to clear a value.
#' @param reason Why the change is made. Required.
#' @param data A data frame with one record per row.
#' @param user Name of the person making the change. It is recorded in the
#'   audit trail but not authenticated.
#' @param lang Language of validation messages: `"en"` or `"es"`.
#' @return The identifier (invisibly) for the single-record functions, and the
#'   number of records added (invisibly) for `registry_import()`.
#' @seealso [registry_data()], [registry_audit()]
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' reg <- registry_create(path, load_template("t2d"), user = "demo")
#'
#' registry_add(
#'   reg,
#'   list(record_id = "SYN-0001", visit_date = "2024-05-01",
#'        sex_at_birth = "female", age_years = 60),
#'   user = "demo"
#' )
#' registry_update(reg, "SYN-0001", list(age_years = 61),
#'                 user = "demo", reason = "Corrected after source check")
#' registry_data(reg)
#'
#' # Invalid data are refused, and nothing is written:
#' try(registry_add(reg, list(record_id = "SYN-0002", visit_date = "2024-05-01",
#'                            sex_at_birth = "female", age_years = 250),
#'                  user = "demo", lang = "es"))
#'
#' registry_close(reg)
#' unlink(path)
#' @export
registry_add <- function(reg, record, user, lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_registry(reg)
  check_user(user)
  vals <- normalize_record(record, reg$dict)
  issues <- validate_record(vals, reg$dict, lang)
  if (nrow(issues) > 0L) stop_validation(issues)

  idvar <- id_variable(reg$dict)
  id <- vals[[idvar]]
  if (is.na(id)) abort("The record identifier '%s' is required.", idvar)

  with_write(reg, {
    if (record_exists(reg$con, id)) {
      stop_validation(make_issue(1L, id, idvar, "duplicate_id",
                                 registat_message("duplicate_id", lang, id)))
    }
    insert_record(reg, vals, user, reason = NA_character_)
  })
  invisible(id)
}

#' @rdname registry_add
#' @export
registry_update <- function(reg, id, changes, user, reason,
                            lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_registry(reg)
  check_user(user)
  check_reason(reason)
  id <- as_scalar_id(id)
  if (!is.list(changes) || length(changes) == 0L ||
      is.null(names(changes)) || anyDuplicated(names(changes))) {
    abort("'changes' must be a named list with at least one variable.")
  }
  nms <- variable_names(reg$dict)
  unknown <- setdiff(names(changes), nms)
  if (length(unknown) > 0L) {
    abort("Unknown variable(s): %s", paste(unknown, collapse = ", "))
  }
  if (id_variable(reg$dict) %in% names(changes)) {
    abort("The record identifier cannot be changed.")
  }

  with_write(reg, {
    row <- fetch_record(reg$con, id)
    if (is.null(row)) abort("Record '%s' was not found.", id)
    if (row$deleted == 1L) {
      abort("Record '%s' has been deleted and cannot be updated.", id)
    }
    cur <- values_from_json(row$data, nms)
    new <- cur
    for (nm in names(changes)) {
      new[[nm]] <- scalar_to_string(changes[[nm]], nm)
    }
    differs <- names(cur)[xor(is.na(cur), is.na(new)) |
                            (!is.na(cur) & !is.na(new) & cur != new)]
    if (length(differs) == 0L) {
      abort("No changes: the new values are identical to the current ones.")
    }
    issues <- validate_record(new, reg$dict, lang)
    if (nrow(issues) > 0L) stop_validation(issues)

    diff_list <- stats::setNames(
      lapply(differs, function(nm) list(old = cur[[nm]], new = new[[nm]])),
      differs
    )
    a <- append_audit(reg$con, user, "update", id, to_json(diff_list), reason)
    DBI::dbExecute(
      reg$con,
      paste("UPDATE records SET data = ?, updated_at = ?,",
            "version = version + 1 WHERE record_id = ?"),
      params = list(to_json(as.list(new)), a$ts, id)
    )
  })
  invisible(id)
}

#' @rdname registry_add
#' @export
registry_delete <- function(reg, id, user, reason) {
  check_registry(reg)
  check_user(user)
  check_reason(reason)
  id <- as_scalar_id(id)
  with_write(reg, {
    row <- fetch_record(reg$con, id)
    if (is.null(row)) abort("Record '%s' was not found.", id)
    if (row$deleted == 1L) abort("Record '%s' is already deleted.", id)
    a <- append_audit(reg$con, user, "delete", id, "{}", reason)
    DBI::dbExecute(
      reg$con,
      paste("UPDATE records SET deleted = 1, updated_at = ?,",
            "version = version + 1 WHERE record_id = ?"),
      params = list(a$ts, id)
    )
  })
  invisible(id)
}

#' @rdname registry_add
#' @export
registry_import <- function(reg, data, user, lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_registry(reg)
  check_user(user)
  if (!is.data.frame(data) || nrow(data) == 0L) {
    abort("'data' must be a data frame with at least one row.")
  }
  idvar <- id_variable(reg$dict)
  chr <- normalize_table(data, reg$dict)
  issues <- validate_data(chr, reg$dict, lang)
  if (nrow(issues) > 0L) stop_validation(issues)
  ids <- chr[[idvar]]
  if (anyNA(ids)) abort("The record identifier '%s' is required.", idvar)
  rows <- lapply(seq_len(nrow(chr)), function(i) unlist(chr[i, , drop = FALSE]))

  with_write(reg, {
    existing <- DBI::dbGetQuery(reg$con, "SELECT record_id FROM records")$record_id
    clash <- which(ids %in% existing)
    if (length(clash) > 0L) {
      stop_validation(make_issue(
        clash, ids[clash], idvar, "duplicate_id",
        vapply(ids[clash], function(x) registat_message("duplicate_id", lang, x),
               character(1))
      ))
    }
    for (i in seq_along(rows)) {
      insert_record(reg, rows[[i]], user, reason = "import")
    }
  })
  invisible(length(rows))
}

#' Check data before importing them
#'
#' Runs exactly the checks of [registry_import()] and reports every problem,
#' without writing anything: validation against the dictionary plus
#' identifiers that already exist in the registry (deleted records included)
#' or are missing.
#'
#' @param reg A registry from [registry_open()] or [registry_create()].
#' @param data A data frame whose columns are named by dictionary variables.
#' @param lang Language of the messages: `"en"` or `"es"`.
#' @return A data frame with columns `row`, `record_id`, `variable`, `rule`
#'   and `message`; zero rows when the data can be imported. `row` is the
#'   position in `data`.
#' @seealso [registry_import()], [validate_data()]
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' reg <- registry_create(path, load_template("t2d"), user = "demo")
#' sim <- synthetic_t2d(20, seed = 1, error_rate = 0.2)
#' nrow(registry_check(reg, sim$data))
#' registry_close(reg)
#' unlink(path)
#' @export
registry_check <- function(reg, data, lang = c("en", "es")) {
  lang <- match.arg(lang)
  check_registry(reg)
  if (!is.data.frame(data) || nrow(data) == 0L) {
    abort("'data' must be a data frame with at least one row.")
  }
  idvar <- id_variable(reg$dict)
  chr <- normalize_table(data, reg$dict)
  issues <- validate_data(chr, reg$dict, lang)
  ids <- chr[[idvar]]

  extra <- list()
  missing_id <- which(is.na(ids))
  if (length(missing_id) > 0L && !any(issues$variable == idvar &
                                      issues$rule == "required")) {
    extra[[1L]] <- make_issue(missing_id, NA_character_, idvar, "required",
                              registat_message("required", lang))
  }
  existing <- DBI::dbGetQuery(reg$con, "SELECT record_id FROM records")$record_id
  clash <- which(!is.na(ids) & ids %in% existing)
  if (length(clash) > 0L) {
    extra[[length(extra) + 1L]] <- make_issue(
      clash, ids[clash], idvar, "duplicate_id",
      vapply(ids[clash], function(x) registat_message("duplicate_id", lang, x),
             character(1)))
  }
  out <- do.call(rbind, c(list(issues), extra))
  out <- out[order(out$row, out$variable, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Read the records and the audit trail
#'
#' `registry_data()` returns the records as a data frame whose columns follow
#' the order and types of the dictionary (`integer`, `numeric`, `Date`, or
#' character). `registry_audit()` returns the audit trail.
#'
#' @param reg A registry from [registry_open()] or [registry_create()].
#' @param include_deleted If `TRUE`, soft-deleted records are included and a
#'   logical column `.deleted` is added.
#' @param record_id If given, only the audit entries of that record.
#' @return A data frame. The audit table has columns `seq`, `timestamp`
#'   (UTC), `user`, `action` (`create`, `add`, `update` or `delete`),
#'   `record_id`, `reason`, `changes` (JSON) and `hash`.
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' reg <- registry_create(path, load_template("t2d"), user = "demo")
#' sim <- synthetic_t2d(n = 20, seed = 1)
#' registry_import(reg, sim$data, user = "demo")
#' head(registry_data(reg), 3)
#' registry_audit(reg)[1:3, c("seq", "action", "record_id")]
#' registry_close(reg)
#' unlink(path)
#' @export
registry_data <- function(reg, include_deleted = FALSE) {
  check_registry(reg)
  q <- "SELECT data, deleted FROM records"
  if (!isTRUE(include_deleted)) q <- paste(q, "WHERE deleted = 0")
  rows <- DBI::dbGetQuery(reg$con, paste(q, "ORDER BY created_seq"))

  nms <- variable_names(reg$dict)
  types <- vapply(reg$dict$variables, function(v) v$type, character(1))
  k <- length(nms)
  mat <- if (nrow(rows) == 0L) {
    matrix(character(), nrow = 0L, ncol = k)
  } else {
    matrix(unlist(lapply(rows$data, values_from_json, nms = nms)),
           ncol = k, byrow = TRUE)
  }

  cols <- lapply(seq_len(k), function(j) typed_column(mat[, j], types[j]))
  names(cols) <- nms
  out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  if (isTRUE(include_deleted)) out$.deleted <- rows$deleted == 1L
  out
}

#' @rdname registry_data
#' @export
registry_audit <- function(reg, record_id = NULL) {
  check_registry(reg)
  sql <- paste("SELECT seq, ts AS timestamp, actor AS user, action,",
               "record_id, reason, changes, hash FROM audit")
  out <- if (is.null(record_id)) {
    DBI::dbGetQuery(reg$con, paste(sql, "ORDER BY seq"))
  } else {
    DBI::dbGetQuery(reg$con, paste(sql, "WHERE record_id = ? ORDER BY seq"),
                    params = list(as_scalar_id(record_id)))
  }
  out
}

# ---- internal helpers -------------------------------------------------------

#' @noRd
check_registry <- function(reg) {
  if (!inherits(reg, "registat_registry")) {
    abort("'reg' must come from registry_create() or registry_open().")
  }
  if (!DBI::dbIsValid(reg$con)) abort("The registry connection is closed.")
  invisible(TRUE)
}

#' @noRd
check_user <- function(user) {
  if (!is.character(user) || length(user) != 1L || is.na(user) ||
      !nzchar(trimws(user))) {
    abort("'user' must be a non-empty string identifying who makes the change.")
  }
  invisible(TRUE)
}

#' @noRd
check_reason <- function(reason) {
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) ||
      !nzchar(trimws(reason))) {
    abort("'reason' is required: explain why the record is changed.")
  }
  invisible(TRUE)
}

#' @noRd
as_scalar_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    abort("'id' must be a single non-empty string.")
  }
  id
}

#' Convert one value to its stored text form (internal)
#' @noRd
scalar_to_string <- function(x, nm) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  if (length(x) > 1L) abort("Variable '%s' must have a single value.", nm)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x)) NA_character_ else x
}

#' Turn a user-supplied record into a named character vector (internal)
#' @noRd
normalize_record <- function(rec, dict) {
  if (is.data.frame(rec)) {
    if (nrow(rec) != 1L) abort("A record must have exactly one row.")
    rec <- as.list(rec)
  }
  if (!is.list(rec) || (length(rec) > 0L && is.null(names(rec)))) {
    abort("A record must be a named list or a one-row data frame.")
  }
  nms <- variable_names(dict)
  if (anyDuplicated(names(rec))) abort("A record has duplicated variable names.")
  unknown <- setdiff(names(rec), nms)
  if (length(unknown) > 0L) {
    abort("Unknown variable(s): %s", paste(unknown, collapse = ", "))
  }
  out <- stats::setNames(rep(NA_character_, length(nms)), nms)
  for (nm in names(rec)) out[[nm]] <- scalar_to_string(rec[[nm]], nm)
  out
}

#' Normalise every column of a table to trimmed text (internal)
#'
#' Columns that the dictionary defines but the table lacks become all `NA`;
#' columns that the dictionary does not define are an error.
#' @noRd
normalize_table <- function(data, dict) {
  nms <- variable_names(dict)
  unknown <- setdiff(names(data), nms)
  if (length(unknown) > 0L) {
    abort("Unknown variable(s): %s", paste(unknown, collapse = ", "))
  }
  cols <- lapply(nms, function(nm) {
    x <- data[[nm]]
    if (is.null(x)) return(rep(NA_character_, nrow(data)))
    if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
    x <- trimws(as.character(x))
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    x
  })
  names(cols) <- nms
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Validate one normalised record (internal)
#' @noRd
validate_record <- function(vals, dict, lang) {
  df <- as.data.frame(as.list(vals), stringsAsFactors = FALSE,
                      check.names = FALSE)
  validate_data(df, dict, lang)
}

#' @noRd
make_issue <- function(row, record_id, variable, rule, message) {
  data.frame(row = as.integer(row), record_id = record_id, variable = variable,
             rule = rule, message = message, stringsAsFactors = FALSE)
}

#' Raise a classed validation error (internal)
#' @noRd
stop_validation <- function(issues) {
  shown <- utils::head(seq_len(nrow(issues)), 5L)
  lines <- sprintf("[row %s] %s: %s", issues$row[shown], issues$variable[shown],
                   issues$message[shown])
  msg <- sprintf("%d validation problem(s): %s", nrow(issues),
                 paste(lines, collapse = " | "))
  stop(structure(
    class = c("registat_validation_error", "error", "condition"),
    list(message = msg, call = NULL, issues = issues)
  ))
}

#' @noRd
fetch_record <- function(con, id) {
  row <- DBI::dbGetQuery(
    con, "SELECT data, deleted FROM records WHERE record_id = ?",
    params = list(id))
  if (nrow(row) == 0L) NULL else as.list(row[1L, ])
}

#' @noRd
record_exists <- function(con, id) !is.null(fetch_record(con, id))

#' Insert a validated record and its audit entry (internal)
#' @noRd
insert_record <- function(reg, vals, actor, reason) {
  id <- vals[[id_variable(reg$dict)]]
  json <- to_json(as.list(vals))
  a <- append_audit(reg$con, actor, "add", id, json, reason)
  DBI::dbExecute(
    reg$con,
    paste("INSERT INTO records (record_id, data, created_seq, created_at,",
          "updated_at, version, deleted) VALUES (?, ?, ?, ?, ?, 1, 0)"),
    params = list(id, json, a$seq, a$ts, a$ts)
  )
}

#' Convert stored text to the type declared in the dictionary (internal)
#' @noRd
typed_column <- function(x, type) {
  switch(
    type,
    integer = suppressWarnings(as.integer(as.numeric(x))),
    numeric = suppressWarnings(as.numeric(x)),
    date = as.Date(x, format = "%Y-%m-%d"),
    x
  )
}
