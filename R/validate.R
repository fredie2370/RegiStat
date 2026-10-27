#' Validate a data set against a dictionary
#'
#' Checks each variable of `data` against the dictionary and returns every
#' problem found. The checks are: missing columns, required values, type
#' (number, whole number, ISO date `YYYY-MM-DD`), numeric range, allowed
#' choices, duplicated identifiers (every occurrence after the first is
#' flagged) and the cross-field rules of the dictionary. Rules whose inputs
#' are missing or invalid are skipped for that row, so one problem is
#' reported once.
#'
#' @param data A data frame, for example read from a CSV file.
#' @param dict A `registat_dictionary`, from [read_dictionary()] or
#'   [load_template()].
#' @param lang Language of the messages: `"en"` or `"es"`.
#' @return A data frame with columns `row`, `record_id`, `variable`, `rule`
#'   and `message`. It has zero rows when no problem is found. `row` is `NA`
#'   for problems that concern a whole column.
#' @examples
#' dict <- load_template("t2d")
#' sim <- synthetic_t2d(n = 100, seed = 1, error_rate = 0.1)
#' head(validate_data(sim$data, dict, lang = "es"))
#' @export
validate_data <- function(data, dict, lang = c("en", "es")) {
  lang <- match.arg(lang)
  if (!is.data.frame(data)) abort("'data' must be a data frame.")
  if (!inherits(dict, "registat_dictionary")) {
    abort("'dict' must come from read_dictionary() or load_template().")
  }

  n <- nrow(data)
  id_name <- id_variable(dict)
  ids <- if (id_name %in% names(data)) {
    trimws(as.character(data[[id_name]]))
  } else {
    rep(NA_character_, n)
  }
  ids[!is.na(ids) & ids == ""] <- NA_character_

  found <- list()
  push <- function(rows, variable, rule, messages) {
    if (length(rows) == 0L) return(invisible(NULL))
    found[[length(found) + 1L]] <<- data.frame(
      row = as.integer(rows),
      record_id = ids[rows],
      variable = variable,
      rule = rule,
      message = rep_len(messages, length(rows)),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  parsed <- list()
  for (v in dict$variables) {
    nm <- v$name
    if (!(nm %in% names(data))) {
      push(NA_integer_, nm, "missing_column",
           registat_message("missing_column", lang, nm))
      next
    }
    x <- data[[nm]]
    xc <- if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
    xc <- trimws(xc)
    xc[!is.na(xc) & xc == ""] <- NA_character_
    absent <- is.na(xc)

    if (isTRUE(v$required)) {
      push(which(absent), nm, "required", registat_message("required", lang))
    }

    val <- switch(
      v$type,
      integer = ,
      numeric = suppressWarnings(as.numeric(xc)),
      date = as.Date(
        ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", xc), xc, NA_character_),
        format = "%Y-%m-%d"
      ),
      xc
    )

    if (v$type %in% c("integer", "numeric")) {
      bad <- which(!absent & is.na(val))
      push(bad, nm, "type", registat_message("type_numeric", lang, xc[bad]))
      if (identical(v$type, "integer")) {
        nonint <- which(!absent & !is.na(val) & is.finite(val) &
                          val != round(val))
        push(nonint, nm, "type",
             registat_message("type_integer", lang, xc[nonint]))
      }
      ok <- which(!is.na(val))
      lo <- if (is.null(v$min)) -Inf else v$min
      hi <- if (is.null(v$max)) Inf else v$max
      outside <- ok[val[ok] < lo | val[ok] > hi]
      push(outside, nm, "range",
           registat_message("range", lang, xc[outside], format(lo), format(hi)))
    } else if (identical(v$type, "date")) {
      bad <- which(!absent & is.na(val))
      push(bad, nm, "type", registat_message("type_date", lang, xc[bad]))
    } else if (identical(v$type, "categorical")) {
      allowed <- vapply(v$choices, function(ch) ch$value, character(1))
      bad <- which(!absent & !(xc %in% allowed))
      push(bad, nm, "choice", registat_message("choice", lang, xc[bad]))
    }
    parsed[[nm]] <- val
  }

  dup <- which(!is.na(ids) & duplicated(ids))
  push(dup, id_name, "duplicate_id",
       registat_message("duplicate_id", lang, ids[dup]))

  all_names <- variable_names(dict)
  for (r in dict$rules) {
    expr <- check_rule_expr(r$expr, all_names)
    needed <- all.vars(expr)
    if (!all(needed %in% names(parsed))) next
    res <- tryCatch(eval(expr, parsed, baseenv()), error = function(e) NULL)
    if (is.null(res) || !is.logical(res) || length(res) != n) next
    rows <- which(!is.na(res) & !res)
    push(rows, paste(needed, collapse = ","), paste0("rule:", r$id),
         r$message[[lang]])
  }

  if (length(found) == 0L) {
    return(data.frame(row = integer(), record_id = character(),
                      variable = character(), rule = character(),
                      message = character(), stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, found)
  res <- res[order(res$row, res$variable, na.last = TRUE), , drop = FALSE]
  rownames(res) <- NULL
  res
}
