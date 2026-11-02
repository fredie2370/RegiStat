# Helpers shared by the Shiny modules.

.dot <- paste0(" ", intToUtf8(0xb7), " ")

#' Label of a variable in a language (internal)
#' @noRd
var_label <- function(dict, name, lang) {
  for (v in dict$variables) {
    if (identical(v$name, name)) return(v$label[[lang]] %||% v$label[["en"]])
  }
  name
}

#' Labels of all variables, named by variable (internal)
#' @noRd
var_labels <- function(dict, lang) {
  stats::setNames(
    vapply(dict$variables, function(v) v$label[[lang]] %||% v$label[["en"]],
           character(1)),
    variable_names(dict))
}

#' One input widget for a dictionary variable (internal)
#'
#' `value` is the current text (or `NA`/`NULL` for empty).
#' @noRd
field_input <- function(v, id, lang, value = NULL) {
  value <- if (is.null(value) || length(value) == 0L || is.na(value)) "" else
    as.character(value)
  label <- v$label[[lang]] %||% v$label[["en"]]
  if (!is.null(v$unit)) label <- sprintf("%s (%s)", label, v$unit)
  if (isTRUE(v$required)) label <- paste0(label, " *")
  help <- v$description[[lang]]

  widget <- switch(
    v$type,
    categorical = {
      codes <- vapply(v$choices, function(ch) ch$value, character(1))
      labs <- vapply(v$choices, function(ch) ch$label[[lang]] %||% ch$label[["en"]],
                     character(1))
      shiny::selectInput(id, label,
                         choices = c(stats::setNames("", ui_text("choose", lang)),
                                     stats::setNames(codes, labs)),
                         selected = value, selectize = FALSE, width = "100%")
    },
    date = {
      # An empty date makes shiny warn that NA is not a date; that is the
      # documented way to start empty, so the warning is silenced.
      suppressWarnings(shiny::dateInput(
        id, label, value = if (nzchar(value)) value else as.Date(NA),
        format = "yyyy-mm-dd", weekstart = 1, language = lang,
        autoclose = TRUE, width = "100%"))
    },
    integer = , numeric = {
      shiny::tagAppendAttributes(
        shiny::textInput(id, label, value = value, width = "100%"),
        inputmode = if (identical(v$type, "integer")) "numeric" else "decimal")
    },
    shiny::textInput(id, label, value = value, width = "100%")
  )
  if (!is.null(help)) {
    widget <- shiny::tagAppendChild(widget,
                                    shiny::tags$div(class = "form-text", help))
  }
  widget
}

#' The form of all fields (internal)
#' @param skip Variable names to leave out.
#' @noRd
form_fields <- function(dict, ns, lang, prefix = "f_", values = NULL,
                        skip = character()) {
  vars <- Filter(function(v) !(v$name %in% skip), dict$variables)
  fields <- lapply(vars, function(v) {
    field_input(v, ns(paste0(prefix, v$name)), lang, values[[v$name]])
  })
  do.call(bslib::layout_column_wrap, c(list(width = "260px"), fields))
}

#' Collect the form values as a named list of text (internal)
#' @noRd
form_values <- function(input, dict, prefix = "f_", skip = character()) {
  vars <- Filter(function(v) !(v$name %in% skip), dict$variables)
  out <- lapply(vars, function(v) {
    x <- input[[paste0(prefix, v$name)]]
    if (is.null(x) || length(x) == 0L) return(NA_character_)
    if (v$type %in% c("integer", "numeric") && is.character(x)) {
      x <- sub(",", ".", trimws(x), fixed = TRUE)
    }
    scalar_to_string(x, v$name)
  })
  names(out) <- vapply(vars, function(v) v$name, character(1))
  out
}

#' An alert listing validation problems (internal)
#' @noRd
issues_alert <- function(issues, dict, lang, title_key = "capture_problems") {
  labs <- var_labels(dict, lang)
  items <- lapply(seq_len(nrow(issues)), function(i) {
    nm <- issues$variable[i]
    shiny::tags$li(shiny::tags$strong(labs[[nm]] %||% nm), ": ",
                   issues$message[i])
  })
  shiny::tags$div(class = "alert alert-danger", role = "alert",
                  shiny::tags$p(class = "mb-1", ui_text(title_key, lang)),
                  shiny::tags$ul(class = "mb-0", items))
}

#' A simple status alert (internal)
#' @noRd
status_alert <- function(text, kind = c("success", "danger", "warning", "info")) {
  kind <- match.arg(kind)
  shiny::tags$div(class = paste("alert", paste0("alert-", kind)), role = "alert",
                  text)
}

#' Run code and turn its errors into an alert (internal)
#'
#' Validation errors become a list of problems; other errors a plain message.
#' @noRd
guarded <- function(expr, dict, lang) {
  tryCatch(expr,
    registat_validation_error = function(e) {
      issues_alert(e$issues, dict, lang)
    },
    error = function(e) {
      status_alert(ui_text("msg_error", lang, conditionMessage(e)), "danger")
    })
}

#' DataTable with the interface language (internal)
#' @noRd
dt_table <- function(df, lang, selection = "none", page_length = 15L, ...) {
  # The "default" style is used because some DT releases ship no Bootstrap 5
  # style files, which makes the browser request a missing resource.
  DT::datatable(df, rownames = FALSE, selection = selection, style = "default",
                options = list(pageLength = page_length, scrollX = TRUE,
                               language = dt_language(lang)), ...)
}

#' Text for the "changes" column of the audit table (internal)
#'
#' `dict` is the dictionary of the registry, or `NULL` for the case-finding
#' store.
#' @noRd
audit_summary <- function(action, changes, dict, lang) {
  empty <- ui_text("empty_value", lang)
  labs <- if (!is.null(dict)) var_labels(dict, lang) else c(
    review_date = ui_text("scr_col_date", lang),
    found = ui_text("scr_col_found", lang),
    study_code = ui_text("scr_col_code", lang),
    note = ui_text("scr_col_note", lang))
  show <- function(nm, x) {
    if (is.null(x)) return(empty)
    if (identical(nm, "found") && is.null(dict)) {
      return(ui_text(if (as.character(x) %in% c("1", "TRUE")) "scr_found_yes"
                     else "scr_found_no", lang))
    }
    as.character(x)
  }
  parsed <- tryCatch(from_json(changes), error = function(e) NULL)
  if (action %in% c("update", "review_update") && is.list(parsed)) {
    parts <- vapply(names(parsed), function(nm) {
      sprintf("%s: %s -> %s", labs[[nm]] %||% nm,
              show(nm, parsed[[nm]][["old"]]), show(nm, parsed[[nm]][["new"]]))
    }, character(1))
    paste(parts, collapse = "; ")
  } else if (identical(action, "add")) {
    ui_text("audit_added", lang)
  } else if (identical(action, "delete")) {
    ui_text("audit_deleted", lang)
  } else if (identical(action, "create")) {
    ui_text(if (is.null(dict)) "audit_scr_created" else "audit_created", lang)
  } else if (identical(action, "review_add") && is.list(parsed)) {
    ui_text("audit_review_added", lang, parsed$file_no %||% "",
            parsed$review_date %||% "", show("found", parsed$found))
  } else if (identical(action, "review_delete")) {
    ui_text("audit_review_deleted", lang)
  } else if (identical(action, "reflist_import") && is.list(parsed)) {
    ui_text("audit_list_loaded", lang, parsed$version %||% "", parsed$n_items %||% "")
  } else {
    ""
  }
}

#' Translated action name (internal)
#' @noRd
audit_action_label <- function(action, lang) {
  key <- paste0("action_", action)
  vapply(seq_along(action), function(i) {
    if (is.null(ui_strings()[[key[i]]])) action[i] else ui_text(key[i], lang)
  }, character(1))
}
