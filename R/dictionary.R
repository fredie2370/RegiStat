#' Read a data dictionary
#'
#' Reads a YAML data dictionary, checks that it is well formed and returns it
#' as a `registat_dictionary` object.
#'
#' A dictionary has a `variables` list (each with `name`, `type`, bilingual
#' `label`, and optionally `required`, `unit`, `min`, `max`, `choices`) and an
#' optional `rules` list of cross-field checks. Allowed types are `id`,
#' `integer`, `numeric`, `categorical`, `date` and `text`. Exactly one
#' variable must have type `id`; it should hold a pseudonymised study code,
#' never a hospital file number or any direct identifier.
#'
#' Rule expressions must evaluate to `TRUE` for valid rows. Only comparison,
#' arithmetic and logical operators, parentheses and `is.na()` are allowed,
#' so a shared dictionary can never run arbitrary code.
#'
#' @param path Path to a YAML dictionary file (UTF-8).
#' @return A `registat_dictionary` object (a list).
#' @examples
#' dict <- load_template("t2d")
#' dictionary_table(dict, "en")[1:3, ]
#' @export
read_dictionary <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    abort("Dictionary file not found: %s", paste(path, collapse = ", "))
  }
  # Read the bytes as UTF-8 without converting to the native encoding, so that
  # accented characters survive on systems whose locale is not UTF-8.
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  dict <- yaml::yaml.load(paste(lines, collapse = "\n"))
  validate_dictionary(dict)
}

#' Validate the structure of a dictionary
#'
#' @param dict A list, as returned by [yaml::read_yaml()].
#' @return The normalised dictionary, with class `registat_dictionary`.
#'   Stops with an informative error when the dictionary is malformed.
#' @export
validate_dictionary <- function(dict) {
  if (!is.list(dict) || !is.list(dict$variables) ||
      length(dict$variables) == 0L) {
    abort("The dictionary must contain a non-empty 'variables' list.")
  }
  allowed_types <- c("id", "integer", "numeric", "categorical", "date", "text")
  seen <- character()

  for (i in seq_along(dict$variables)) {
    v <- dict$variables[[i]]
    nm <- v$name
    if (!is.character(nm) || length(nm) != 1L ||
        !grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)) {
      abort("Variable #%d has an invalid 'name'.", i)
    }
    if (nm %in% seen) abort("Duplicate variable name: '%s'.", nm)
    seen <- c(seen, nm)
    if (!is.character(v$type) || length(v$type) != 1L ||
        !(v$type %in% allowed_types)) {
      abort("Variable '%s' has an invalid 'type'.", nm)
    }
    check_labels(v$label, sprintf("the label of '%s'", nm))
    dict$variables[[i]]$required <- isTRUE(v$required)

    for (bound in c("min", "max")) {
      if (!is.null(v[[bound]]) && !is.numeric(v[[bound]])) {
        abort("Variable '%s': '%s' must be numeric.", nm, bound)
      }
    }
    if (!is.null(v$min) && !is.null(v$max) && v$min > v$max) {
      abort("Variable '%s': 'min' is greater than 'max'.", nm)
    }

    if (identical(v$type, "categorical")) {
      if (!is.list(v$choices) || length(v$choices) == 0L) {
        abort("Categorical variable '%s' needs a non-empty 'choices' list.", nm)
      }
      values <- character()
      for (ch in v$choices) {
        if (!is.character(ch$value) || length(ch$value) != 1L) {
          abort(paste0(
            "Variable '%s': every choice 'value' must be text. ",
            "Quote values such as \"yes\" and \"no\" in the YAML file."
          ), nm)
        }
        check_labels(ch$label, sprintf("a choice label of '%s'", nm))
        values <- c(values, ch$value)
      }
      if (anyDuplicated(values)) {
        abort("Variable '%s' has duplicated choice values.", nm)
      }
    }
  }

  n_id <- sum(vapply(dict$variables, function(v) identical(v$type, "id"),
                     logical(1)))
  if (n_id != 1L) {
    abort("Exactly one variable must have type 'id' (found %d).", n_id)
  }

  rule_ids <- character()
  for (i in seq_along(dict$rules)) {
    r <- dict$rules[[i]]
    if (!is.character(r$id) || length(r$id) != 1L ||
        !grepl("^[A-Za-z][A-Za-z0-9_]*$", r$id)) {
      abort("Rule #%d has an invalid 'id'.", i)
    }
    if (r$id %in% rule_ids) abort("Duplicate rule id: '%s'.", r$id)
    rule_ids <- c(rule_ids, r$id)
    check_labels(r$message, sprintf("the message of rule '%s'", r$id))
    check_rule_expr(r$expr, seen)
  }

  class(dict) <- c("registat_dictionary", "list")
  dict
}

#' @noRd
check_labels <- function(x, what) {
  ok <- is.list(x) &&
    is.character(x$en) && length(x$en) == 1L && nzchar(x$en) &&
    is.character(x$es) && length(x$es) == 1L && nzchar(x$es)
  if (!ok) abort("Both 'en' and 'es' text are required for %s.", what)
  invisible(TRUE)
}

# Calls that a rule expression may use. Everything else is rejected.
.allowed_calls <- c("<", "<=", ">", ">=", "==", "!=", "+", "-", "*", "/",
                    "&", "|", "!", "(", "is.na")

#' Parse a rule expression and reject anything that is not whitelisted
#' @return The parsed expression.
#' @noRd
check_rule_expr <- function(expr_text, var_names) {
  if (!is.character(expr_text) || length(expr_text) != 1L) {
    abort("A rule 'expr' must be a single text string.")
  }
  parsed <- tryCatch(parse(text = expr_text, keep.source = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed) || length(parsed) != 1L) {
    abort("Rule expression cannot be parsed: %s", expr_text)
  }
  walk <- function(e) {
    if (is.call(e)) {
      fn <- e[[1L]]
      if (!is.symbol(fn) || !(as.character(fn) %in% .allowed_calls)) {
        abort("Function not allowed in rule: %s", paste(deparse(fn), collapse = ""))
      }
      for (arg in as.list(e)[-1L]) walk(arg)
    } else if (is.symbol(e)) {
      if (!(as.character(e) %in% var_names)) {
        abort("Unknown variable in rule: %s", as.character(e))
      }
    } else if (!(is.numeric(e) || is.character(e) || is.logical(e))) {
      abort("Constant type not allowed in rule: %s", expr_text)
    }
    invisible(TRUE)
  }
  walk(parsed[[1L]])
  parsed[[1L]]
}

#' Load a bundled study template
#'
#' @param name Template name. See [list_templates()].
#' @return A `registat_dictionary` object.
#' @examples
#' list_templates()
#' dict <- load_template("t2d")
#' @export
load_template <- function(name = "t2d") {
  path <- system.file("templates", name, "dictionary.yml", package = "RegiStat")
  if (!nzchar(path)) {
    abort("Unknown template '%s'. Available: %s", name,
          paste(list_templates(), collapse = ", "))
  }
  read_dictionary(path)
}

#' List the bundled study templates
#'
#' @return A character vector of template names.
#' @export
list_templates <- function() {
  list.files(system.file("templates", package = "RegiStat"))
}

#' Summarise a dictionary as a table
#'
#' Produces a codebook-style table that can be printed, exported or included
#' in a report.
#'
#' @param dict A `registat_dictionary`.
#' @param lang Language of the labels: `"en"` or `"es"`.
#' @return A data frame with one row per variable.
#' @examples
#' dictionary_table(load_template("t2d"), "es")[1:3, ]
#' @export
dictionary_table <- function(dict, lang = c("en", "es")) {
  lang <- match.arg(lang)
  rows <- lapply(dict$variables, function(v) {
    data.frame(
      variable = v$name,
      type = v$type,
      label = v$label[[lang]],
      unit = if (is.null(v$unit)) NA_character_ else as.character(v$unit),
      required = isTRUE(v$required),
      min = if (is.null(v$min)) NA_real_ else as.numeric(v$min),
      max = if (is.null(v$max)) NA_real_ else as.numeric(v$max),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
