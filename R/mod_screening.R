# Case-finding tab ("Cotejo"): reference list, reviews, summary, integrity.
# The file numbers live in a separate SQLite file next to the registry.

screening_path <- function(reg) {
  file.path(dirname(reg$path), "screening.sqlite")
}

screening_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("body"))
}

screening_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- shiny::reactiveVal(NULL)

    bump <- function() state$stick <- state$stick + 1L

    # Open the case-finding file automatically when it already exists.
    shiny::observe({
      reg <- state$reg
      shiny::req(reg)
      p <- screening_path(reg)
      if (is.null(shiny::isolate(state$scr)) && file.exists(p)) {
        res <- tryCatch(screening_open(p), error = function(e) e)
        if (inherits(res, "error")) {
          status(status_alert(ui_text("msg_error", shiny::isolate(lang()),
                                      conditionMessage(res)), "danger"))
        } else {
          state$scr <- res
        }
      }
    })

    output$body <- shiny::renderUI({
      l <- lang()
      if (is.null(state$scr)) {
        return(shiny::tags$div(
          shiny::tags$p(ui_text("scr_intro", l)),
          shiny::tags$p(class = "text-muted small", ui_text("scr_activate_note", l)),
          shiny::uiOutput(ns("status")),
          shiny::actionButton(ns("activate"), ui_text("scr_activate", l),
                              class = "btn-primary")))
      }
      bslib::navset_pill(
        bslib::nav_panel(ui_text("scr_sub_reviews", l), reviews_panel(ns, l)),
        bslib::nav_panel(ui_text("scr_sub_list", l), list_panel(ns, l)),
        bslib::nav_panel(ui_text("scr_sub_summary", l), summary_panel(ns, l)),
        bslib::nav_panel(ui_text("scr_sub_integrity", l), audit_ui(ns("audit"))))
    })

    output$status <- shiny::renderUI(status())

    shiny::observeEvent(input$activate, {
      l <- lang()
      if (!nzchar(trimws(user() %||% ""))) {
        status(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      res <- tryCatch(screening_create(screening_path(state$reg), user = user()),
                      error = function(e) e)
      if (inherits(res, "error")) {
        status(status_alert(ui_text("msg_error", l, conditionMessage(res)), "danger"))
      } else {
        status(NULL)
        state$scr <- res
      }
    })

    audit_server("audit", state, lang, user, kind = "screening")

    reviews_server(input, output, session, state, lang, user, bump)
    list_server(input, output, session, state, lang, user, bump)
    summary_server(input, output, session, state, lang, bump)
  })
}

# ---- reviews ----------------------------------------------------------------

reviews_panel <- function(ns, l) {
  shiny::tagList(
    shiny::tags$p(class = "text-muted small mt-2", ui_text("scr_privacy", l)),
    shiny::tags$h5(ui_text("scr_review_title", l)),
    shiny::tags$div(
      class = "registat-form",
      bslib::layout_column_wrap(
        width = "230px",
        shiny::textInput(ns("file_no"), paste0(ui_text("scr_file_no", l), " *"),
                         width = "100%"),
        shiny::dateInput(ns("review_date"), paste0(ui_text("scr_review_date", l), " *"),
                         value = Sys.Date(), format = "yyyy-mm-dd", weekstart = 1,
                         language = l, autoclose = TRUE, width = "100%"),
        shiny::radioButtons(ns("found"), paste0(ui_text("scr_found", l), " *"),
                            choices = stats::setNames(c("1", "0"),
                                                      c(ui_text("scr_found_yes", l),
                                                        ui_text("scr_found_no", l))),
                            selected = character(0), inline = TRUE),
        shiny::textInput(ns("study_code"), ui_text("scr_study_code", l), width = "100%"),
        shiny::textInput(ns("note"), ui_text("scr_note", l), width = "100%"))),
    shiny::tags$div(
      class = "d-flex gap-2 my-2",
      shiny::actionButton(ns("save"), ui_text("scr_save", l), class = "btn-primary")),
    shiny::uiOutput(ns("review_status")),
    shiny::tags$div(
      class = "d-flex flex-wrap gap-2 align-items-center mt-3 mb-2",
      shiny::actionButton(ns("edit"), ui_text("records_edit", l),
                          class = "btn-outline-primary"),
      shiny::actionButton(ns("delete"), ui_text("records_delete", l),
                          class = "btn-outline-danger"),
      shiny::downloadButton(ns("download"), ui_text("records_download", l),
                            class = "btn-outline-secondary")),
    DT::DTOutput(ns("reviews_table")))
}

yes_no_unknown <- function(x, l) {
  ifelse(is.na(x), ui_text("scr_unknown", l),
         ifelse(x, ui_text("answer_yes", l), ui_text("answer_no", l)))
}

reviews_server <- function(input, output, session, state, lang, user, bump) {
  ns <- session$ns
  review_status <- shiny::reactiveVal(NULL)
  output$review_status <- shiny::renderUI(review_status())
  shiny::observeEvent(lang(), review_status(NULL), ignoreInit = TRUE)

  reviews <- shiny::reactive({
    state$stick
    shiny::req(state$scr)
    screening_reviews(state$scr)
  })

  shiny::observeEvent(input$save, {
    l <- lang()
    scr <- state$scr
    if (!nzchar(trimws(user() %||% ""))) {
      review_status(status_alert(ui_text("msg_user_required", l), "warning"))
      return()
    }
    id <- NULL
    res <- tryCatch({
      id <- screening_add_review(scr, input$file_no, input$review_date,
                                 input$found %||% NA_character_, user = user(),
                                 study_code = input$study_code, note = input$note,
                                 lang = l)
      NULL
    }, error = function(e) status_alert(ui_text("msg_error", l, conditionMessage(e)),
                                        "danger"))
    if (!is.null(res)) {
      review_status(res)
      return()
    }
    r <- screening_reviews(scr)
    row <- r[r$review_id == id, , drop = FALSE]
    ver <- latest_version(scr$con)
    review_status(if (is.na(row$listed)) {
      status_alert(ui_text("scr_saved_no_list", l), "info")
    } else if (row$listed) {
      status_alert(ui_text("scr_saved_listed", l, row$file_no, ver), "success")
    } else {
      status_alert(ui_text("scr_saved_not_listed", l, row$file_no, ver), "warning")
    })
    for (f in c("file_no", "study_code", "note")) shiny::updateTextInput(session, f, value = "")
    shiny::updateRadioButtons(session, "found", selected = character(0))
    bump()
  })

  shown <- shiny::reactive({
    r <- reviews()
    l <- lang()
    out <- data.frame(
      r$review_id, r$file_no, as.character(r$review_date),
      yes_no_unknown(r$found, l), yes_no_unknown(r$listed, l),
      ifelse(is.na(r$study_code), "", r$study_code),
      ifelse(is.na(r$note), "", r$note), stringsAsFactors = FALSE)
    names(out) <- vapply(c("scr_col_id", "scr_col_file", "scr_col_date",
                           "scr_col_found", "scr_col_listed", "scr_col_code",
                           "scr_col_note"), ui_text, character(1), lang = l)
    out[order(-r$review_id), , drop = FALSE]
  })

  output$reviews_table <- DT::renderDT(dt_table(shown(), lang(), selection = "single"))

  selected_review <- function() {
    row <- input$reviews_table_rows_selected
    if (length(row) != 1L) return(NULL)
    as.integer(shown()[[1L]][row])
  }

  output$download <- shiny::downloadHandler(
    filename = function() {
      paste0("registat-reviews-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      r <- reviews()
      r$review_date <- as.character(r$review_date)
      r$found <- ifelse(r$found, "found", "not_found")
      r$listed <- ifelse(is.na(r$listed), "", ifelse(r$listed, "yes", "no"))
      write_csv_excel(r, file)
    })

  # ---- edit
  editing <- shiny::reactiveVal(NULL)
  edit_status <- shiny::reactiveVal(NULL)
  output$edit_status <- shiny::renderUI(edit_status())

  shiny::observeEvent(input$edit, {
    l <- lang()
    review_status(NULL)
    rid <- selected_review()
    if (is.null(rid)) {
      review_status(status_alert(ui_text("records_select_first", l), "info"))
      return()
    }
    row <- reviews()[reviews()$review_id == rid, , drop = FALSE]
    editing(list(id = rid, row = row))
    edit_status(NULL)
    shiny::showModal(shiny::modalDialog(
      title = ui_text("scr_edit_title", l, rid), easyClose = FALSE,
      shiny::uiOutput(ns("edit_status")),
      shiny::tags$p(class = "text-muted", shiny::tags$strong(ui_text("scr_file_no", l)),
                    ": ", row$file_no),
      shiny::dateInput(ns("e_date"), ui_text("scr_review_date", l),
                       value = row$review_date, format = "yyyy-mm-dd",
                       weekstart = 1, language = l, autoclose = TRUE),
      shiny::radioButtons(ns("e_found"), ui_text("scr_found", l),
                          choices = stats::setNames(c("1", "0"),
                                                    c(ui_text("scr_found_yes", l),
                                                      ui_text("scr_found_no", l))),
                          selected = if (row$found) "1" else "0", inline = TRUE),
      shiny::textInput(ns("e_code"), ui_text("scr_study_code", l),
                       value = row$study_code %||% "", width = "100%"),
      shiny::textInput(ns("e_note"), ui_text("scr_note", l),
                       value = row$note %||% "", width = "100%"),
      shiny::textInput(ns("e_reason"), ui_text("edit_reason", l), width = "100%"),
      footer = shiny::tagList(
        shiny::modalButton(ui_text("cancel", l)),
        shiny::actionButton(ns("edit_save"), ui_text("edit_save", l),
                            class = "btn-primary"))))
  })

  shiny::observeEvent(input$edit_save, {
    info <- editing()
    shiny::req(info)
    l <- lang()
    if (!nzchar(trimws(user() %||% ""))) {
      edit_status(status_alert(ui_text("msg_user_required", l), "warning"))
      return()
    }
    if (!nzchar(trimws(input$e_reason %||% ""))) {
      edit_status(status_alert(ui_text("reason_required", l), "warning"))
      return()
    }
    changes <- list(review_date = input$e_date, found = input$e_found,
                    study_code = input$e_code, note = input$e_note)
    res <- tryCatch({
      screening_update_review(state$scr, info$id, changes, user = user(),
                              reason = input$e_reason, lang = l)
      NULL
    }, error = function(e) status_alert(ui_text("msg_error", l, conditionMessage(e)),
                                        "danger"))
    if (is.null(res)) {
      shiny::removeModal()
      review_status(status_alert(ui_text("scr_edit_saved", l, info$id), "success"))
      editing(NULL)
      bump()
    } else {
      edit_status(res)
    }
  })

  # ---- delete
  deleting <- shiny::reactiveVal(NULL)
  delete_status <- shiny::reactiveVal(NULL)
  output$delete_status <- shiny::renderUI(delete_status())

  shiny::observeEvent(input$delete, {
    l <- lang()
    review_status(NULL)
    delete_status(NULL)
    rid <- selected_review()
    if (is.null(rid)) {
      review_status(status_alert(ui_text("records_select_first", l), "info"))
      return()
    }
    deleting(rid)
    shiny::showModal(shiny::modalDialog(
      title = ui_text("scr_delete_title", l, rid), easyClose = FALSE,
      shiny::tags$p(ui_text("delete_explain", l)),
      shiny::uiOutput(ns("delete_status")),
      shiny::textInput(ns("delete_reason"), ui_text("delete_reason", l), width = "100%"),
      footer = shiny::tagList(
        shiny::modalButton(ui_text("cancel", l)),
        shiny::actionButton(ns("delete_confirm"), ui_text("delete_confirm", l),
                            class = "btn-danger"))))
  })

  shiny::observeEvent(input$delete_confirm, {
    rid <- deleting()
    shiny::req(rid)
    l <- lang()
    if (!nzchar(trimws(user() %||% ""))) {
      delete_status(status_alert(ui_text("msg_user_required", l), "warning"))
      return()
    }
    if (!nzchar(trimws(input$delete_reason %||% ""))) {
      delete_status(status_alert(ui_text("reason_required", l), "warning"))
      return()
    }
    res <- tryCatch({
      screening_delete_review(state$scr, rid, user = user(),
                              reason = input$delete_reason, lang = l)
      NULL
    }, error = function(e) status_alert(ui_text("msg_error", l, conditionMessage(e)),
                                        "danger"))
    if (is.null(res)) {
      shiny::removeModal()
      review_status(status_alert(ui_text("scr_delete_done", l, rid), "success"))
      deleting(NULL)
      bump()
    } else {
      delete_status(res)
    }
  })
}

# ---- reference list ---------------------------------------------------------

list_panel <- function(ns, l) {
  shiny::tagList(
    shiny::tags$div(class = "mt-2", shiny::uiOutput(ns("list_info"))),
    shiny::fileInput(ns("list_file"), ui_text("scr_list_file", l),
                     buttonLabel = ui_text("import_browse", l), placeholder = "",
                     accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
    shiny::uiOutput(ns("list_options")),
    shiny::uiOutput(ns("list_preview")),
    shiny::uiOutput(ns("list_status")),
    shiny::tags$h5(class = "mt-3", ui_text("scr_list_versions", l)),
    DT::DTOutput(ns("versions_table")),
    shiny::uiOutput(ns("changes")))
}

list_server <- function(input, output, session, state, lang, user, bump) {
  ns <- session$ns
  list_status <- shiny::reactiveVal(NULL)
  output$list_status <- shiny::renderUI(list_status())

  versions <- shiny::reactive({
    state$stick
    shiny::req(state$scr)
    screening_lists(state$scr)
  })

  output$list_info <- shiny::renderUI({
    l <- lang()
    v <- versions()
    if (nrow(v) == 0L) {
      return(shiny::tags$p(class = "text-muted", ui_text("scr_list_none", l)))
    }
    cur <- v[nrow(v), ]
    n_pending <- length(screening_pending(state$scr))
    shiny::tagList(
      shiny::tags$p(class = "mb-1", shiny::tags$strong(
        ui_text("scr_list_current", l, cur$version, cur$loaded_at, cur$n_items))),
      shiny::tags$p(class = "text-muted", ui_text("scr_list_pending", l, n_pending)))
  })

  list_path <- shiny::reactive({
    f <- input$list_file
    shiny::req(f)
    dest <- paste0(f$datapath, ".", tolower(tools::file_ext(f$name)))
    if (!file.exists(dest)) file.copy(f$datapath, dest)
    dest
  })

  output$list_options <- shiny::renderUI({
    shiny::req(input$list_file)
    l <- lang()
    sh <- tryCatch(table_sheets(list_path()), error = function(e) NULL)
    shiny::tagList(
      if (!is.null(sh)) shiny::selectInput(ns("list_sheet"), ui_text("import_sheet", l),
                                           choices = sh, selectize = FALSE),
      bslib::layout_column_wrap(
        width = "230px",
        shiny::textInput(ns("list_column"), ui_text("scr_list_column", l), value = "A"),
        shiny::numericInput(ns("list_first_row"), ui_text("scr_list_first_row", l),
                            value = 1, min = 1, step = 1),
        shiny::textInput(ns("list_separator"), ui_text("scr_list_separator", l),
                         value = "/")))
  })

  tokens <- shiny::reactive({
    shiny::req(input$list_file, input$list_column, input$list_first_row)
    p <- list_path()
    is_excel <- tolower(tools::file_ext(p)) %in% c("xlsx", "xls")
    if (is_excel) shiny::req(input$list_sheet)
    tryCatch(
      read_reference_list(p, sheet = if (is_excel) input$list_sheet else 1,
                          column = input$list_column,
                          first_row = input$list_first_row,
                          separator = input$list_separator %||% ""),
      error = function(e) e)
  })

  output$list_preview <- shiny::renderUI({
    tk <- tokens()
    l <- lang()
    if (inherits(tk, "error")) {
      return(status_alert(ui_text("msg_error", l, conditionMessage(tk)), "danger"))
    }
    items <- sort(unique(stats::na.omit(normalize_file_no(tk, state$scr$norm))))
    if (length(items) == 0L) {
      return(status_alert(ui_text("scr_list_preview_none", l), "warning"))
    }
    shiny::tagList(
      shiny::tags$p(ui_text("scr_list_preview", l, length(items),
                            paste(utils::head(items, 5L), collapse = ", "))),
      shiny::actionButton(ns("list_load"), ui_text("scr_list_load", l),
                          class = "btn-primary"))
  })

  shiny::observeEvent(input$list_load, {
    l <- lang()
    if (!nzchar(trimws(user() %||% ""))) {
      list_status(status_alert(ui_text("msg_user_required", l), "warning"))
      return()
    }
    tk <- tokens()
    shiny::req(!inherits(tk, "error"))
    res <- tryCatch({
      ver <- screening_load_list(
        state$scr, tk, user = user(), source_name = input$list_file$name,
        config = list(sheet = input$list_sheet, column = input$list_column,
                      first_row = input$list_first_row,
                      separator = input$list_separator),
        lang = l)
      n <- screening_lists(state$scr)$n_items
      list(ver = ver, n = n[length(n)])
    }, error = function(e) e)
    if (inherits(res, "error")) {
      list_status(status_alert(ui_text("msg_error", l, conditionMessage(res)), "danger"))
    } else {
      list_status(status_alert(ui_text("scr_list_loaded", l, res$ver, res$n), "success"))
      bump()
    }
  })

  output$versions_table <- DT::renderDT({
    v <- versions()
    l <- lang()
    out <- data.frame(v$version, v$loaded_at, ifelse(is.na(v$source_name), "", v$source_name),
                      v$n_items, stringsAsFactors = FALSE)
    names(out) <- vapply(c("col_version", "col_loaded", "col_source", "col_items"),
                         ui_text, character(1), lang = l)
    dt_table(out[order(-v$version), , drop = FALSE], l, page_length = 5L)
  })

  output$changes <- shiny::renderUI({
    v <- versions()
    if (nrow(v) < 2L) return(NULL)
    l <- lang()
    ch <- screening_changes(state$scr, lang = l)
    shiny::tagList(
      shiny::tags$h5(class = "mt-3", ui_text("scr_changes_title", l, ch$from, ch$to)),
      shiny::tags$p(ui_text("scr_changes_counts", l, length(ch$added), length(ch$removed))),
      shiny::tags$h6(ui_text("scr_changes_reviews", l)),
      if (nrow(ch$reviews) == 0L) {
        shiny::tags$p(class = "text-muted", ui_text("scr_changes_none", l))
      } else {
        shiny::tags$ul(lapply(seq_len(nrow(ch$reviews)), function(i) {
          r <- ch$reviews[i, ]
          shiny::tags$li(sprintf("%s (%s): %s -> %s", r$file_no,
                                 as.character(r$review_date),
                                 yes_no_unknown(r$listed_from, l),
                                 yes_no_unknown(r$listed_to, l)))
        }))
      })
  })
}

# ---- summary ----------------------------------------------------------------

summary_panel <- function(ns, l) {
  shiny::tagList(
    shiny::tags$h5(class = "mt-2", ui_text("scr_summary_title", l)),
    shiny::tags$p(class = "text-muted small", ui_text("scr_summary_note", l)),
    DT::DTOutput(ns("summary_table")))
}

summary_server <- function(input, output, session, state, lang, bump) {
  output$summary_table <- DT::renderDT({
    state$stick
    shiny::req(state$scr)
    l <- lang()
    s <- screening_summary(state$scr)
    out <- data.frame(s$month, yes_no_unknown(s$found, l), yes_no_unknown(s$listed, l),
                      s$n, stringsAsFactors = FALSE)
    names(out) <- c(ui_text("col_month", l), ui_text("scr_col_found", l),
                    ui_text("scr_col_listed", l), ui_text("col_n", l))
    dt_table(out, l)
  })
}
