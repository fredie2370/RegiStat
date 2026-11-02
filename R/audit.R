# Audit trail internals and verification.
#
# Every change to a registry is appended to the `audit` table. Each entry
# stores the SHA-256 hash of its own content plus the hash of the previous
# entry, forming a chain. Editing or removing an entry in the middle of the
# chain breaks it. See registry_verify() for what this does and does not
# guarantee.

.genesis_hash <- strrep("0", 64L)
.schema_version <- "1"

#' @noRd
sha256 <- function(x) {
  digest::digest(charToRaw(enc2utf8(x)), algo = "sha256", serialize = FALSE)
}

#' @noRd
now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

#' @noRd
to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null",
                                null = "null", digits = NA))
}

#' @noRd
from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

#' Hash of one audit entry (internal)
#' @noRd
entry_hash <- function(seq, ts, actor, action, record_id, changes, reason,
                       prev_hash) {
  parts <- c(as.character(as.integer(seq)), ts, actor, action,
             as.character(record_id), changes, as.character(reason), prev_hash)
  parts[is.na(parts)] <- ""
  sha256(paste(parts, collapse = "\u001f"))
}

#' Append an entry to the audit chain (internal)
#'
#' Must be called inside a write transaction.
#' @return A list with the new `seq` and its timestamp `ts`.
#' @noRd
append_audit <- function(con, actor, action, record_id, changes, reason) {
  last <- DBI::dbGetQuery(
    con, "SELECT seq, hash FROM audit ORDER BY seq DESC LIMIT 1"
  )
  seq <- if (nrow(last) == 0L) 1L else as.integer(last$seq) + 1L
  prev <- if (nrow(last) == 0L) .genesis_hash else last$hash
  ts <- now_utc()
  record_id <- as.character(record_id)
  reason <- as.character(reason)
  h <- entry_hash(seq, ts, actor, action, record_id, changes, reason, prev)
  DBI::dbExecute(
    con,
    paste("INSERT INTO audit",
          "(seq, ts, actor, action, record_id, changes, reason, prev_hash, hash)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"),
    params = list(seq, ts, actor, action, record_id, changes, reason, prev, h)
  )
  list(seq = seq, ts = ts)
}

#' Run code inside a write transaction (internal)
#'
#' Takes the write lock immediately, so concurrent writers wait instead of
#' interleaving, and rolls everything back if `expr` fails.
#' @noRd
with_write <- function(reg, expr) {
  con <- reg$con
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE),
          add = TRUE)
  res <- expr
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  res
}

#' Read the values of a stored record as a named character vector (internal)
#' @noRd
values_from_json <- function(txt, nms) {
  parsed <- from_json(txt)
  out <- vapply(nms, function(nm) {
    v <- parsed[[nm]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1))
  names(out) <- nms
  out
}

#' Verify the integrity of a registry
#'
#' Performs two checks:
#'
#' 1. **Chain check.** Entries must be numbered without gaps, each must point
#'    to the hash of the previous one, and each stored hash must match the
#'    entry's content.
#' 2. **Replay check.** The audit trail is replayed from the start and the
#'    resulting records must match the `records` table exactly. This detects
#'    records edited, added or removed directly in the database file.
#'
#' @section What this guarantees, and what it does not:
#' The audit trail is *tamper-evident*, not *tamper-proof*. Someone with write
#' access to the database file who understands the format can rewrite the
#' whole chain consistently, and removing the **last** entries leaves a valid
#' chain. To detect that, record the `head` value (the hash of the last entry)
#' somewhere outside the database, for example in the study notebook or in an
#' email to yourself, and pass it later as `expected_head`.
#'
#' User names are self-declared: this version has no authentication.
#'
#' @param reg A registry from [registry_open()] or [registry_create()].
#' @param expected_head Optional hash saved earlier from a verification. When
#'   given, the current head must equal it.
#' @return A `registat_verification` object, a list with `ok` (logical),
#'   `n_entries`, `head` and `problems` (a character vector, empty when `ok`).
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' reg <- registry_create(path, load_template("t2d"), user = "demo")
#' v <- registry_verify(reg)
#' v$ok
#' registry_close(reg)
#' unlink(path)
#' @export
registry_verify <- function(reg, expected_head = NULL) {
  check_registry(reg)
  a <- DBI::dbGetQuery(
    reg$con,
    paste("SELECT seq, ts, actor, action, record_id, changes, reason,",
          "prev_hash, hash FROM audit ORDER BY seq")
  )
  n <- nrow(a)
  problems <- chain_problems(a)

  problems <- c(problems, replay_problems(reg, a))

  head <- if (n > 0L) a$hash[n] else .genesis_hash
  if (!is.null(expected_head) && !identical(as.character(expected_head), head)) {
    problems <- c(problems, paste(
      "The head of the audit chain does not match the expected value:",
      "entries may have been removed or rewritten."))
  }

  structure(
    list(ok = length(problems) == 0L, n_entries = n, head = head,
         problems = problems),
    class = "registat_verification"
  )
}

#' Check the hash chain of an audit table (internal)
#'
#' Entries must be numbered without gaps, each must point to the hash of the
#' previous one, and each stored hash must match the entry's content.
#' @param a A data frame with the audit columns, ordered by `seq`.
#' @return A character vector of problems.
#' @noRd
chain_problems <- function(a) {
  problems <- character()
  prev <- .genesis_hash
  for (i in seq_len(nrow(a))) {
    if (a$seq[i] != i) {
      problems <- c(problems, sprintf(
        "Audit entry %d: sequence gap or reordering (found number %d).",
        i, a$seq[i]))
    }
    if (a$prev_hash[i] != prev) {
      problems <- c(problems, sprintf(
        "Audit entry %d: broken link to the previous entry.", a$seq[i]))
    }
    h <- entry_hash(a$seq[i], a$ts[i], a$actor[i], a$action[i],
                    a$record_id[i], a$changes[i], a$reason[i], a$prev_hash[i])
    if (h != a$hash[i]) {
      problems <- c(problems, sprintf(
        "Audit entry %d: content altered (hash does not match).", a$seq[i]))
    }
    prev <- a$hash[i]
  }
  problems
}

#' Replay the audit trail and compare it with the records table (internal)
#' @return A character vector of problems.
#' @noRd
replay_problems <- function(reg, a) {
  problems <- character()
  nms <- variable_names(reg$dict)
  state <- list()

  for (i in seq_len(nrow(a))) {
    act <- a$action[i]
    id <- a$record_id[i]
    ok <- tryCatch({
      if (identical(act, "add")) {
        state[[id]] <- list(values = values_from_json(a$changes[i], nms),
                            deleted = FALSE)
      } else if (identical(act, "update")) {
        parsed <- from_json(a$changes[i])
        v <- state[[id]]$values
        for (nm in names(parsed)) {
          nv <- parsed[[nm]][["new"]]
          v[[nm]] <- if (is.null(nv)) NA_character_ else as.character(nv)
        }
        state[[id]]$values <- v
      } else if (identical(act, "delete")) {
        state[[id]]$deleted <- TRUE
      }
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      problems <- c(problems, sprintf(
        "Audit entry %d could not be replayed.", a$seq[i]))
    }
  }

  recs <- DBI::dbGetQuery(reg$con,
                          "SELECT record_id, data, deleted FROM records")
  for (id in setdiff(recs$record_id, names(state))) {
    problems <- c(problems, sprintf(
      "Record '%s' exists in the table but not in the audit trail.", id))
  }
  for (id in setdiff(names(state), recs$record_id)) {
    problems <- c(problems, sprintf(
      "Record '%s' is in the audit trail but missing from the table.", id))
  }
  for (j in seq_len(nrow(recs))) {
    id <- recs$record_id[j]
    if (!(id %in% names(state))) next
    stored <- tryCatch(values_from_json(recs$data[j], nms),
                       error = function(e) NULL)
    if (is.null(stored) ||
        !identical(unname(stored), unname(state[[id]]$values))) {
      problems <- c(problems, sprintf(
        "Record '%s' does not match its audit trail (edited outside RegiStat?).",
        id))
    }
    if ((recs$deleted[j] == 1L) != isTRUE(state[[id]]$deleted)) {
      problems <- c(problems, sprintf(
        "Record '%s': deleted flag does not match its audit trail.", id))
    }
  }
  problems
}

#' @export
print.registat_verification <- function(x, ...) {
  cat("<RegiStat audit verification>\n")
  cat(sprintf("Entries checked: %d\nHead: %s\n", x$n_entries, x$head))
  if (x$ok) {
    cat("Result: OK, no problems found.\n")
  } else {
    cat("Result: PROBLEMS FOUND\n")
    cat(paste0(" - ", x$problems), sep = "\n")
  }
  invisible(x)
}
