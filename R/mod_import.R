# Import tab: file -> column matching -> check -> import.

import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("intro")),
    shiny::uiOutput(ns("file_controls")),
    shiny::uiOutput(ns("options")),
    shiny::uiOutput(ns("mapping")),
    shiny::uiOutput(ns("actions")),
    shiny::uiOutput(ns("result")),
    DT::DTOutput(ns("problems"))
  )
}

import_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ready <- shiny::reactiveVal(NULL)   # converted data, only after a clean check
    result <- shiny::reactiveVal(NULL)
    issues <- shiny::reactiveVal(NULL)

    output$intro <- shiny::renderUI({
      shiny::tags$div(shiny::tags$h4(ui_text("import_title", lang())),
                      shiny::tags$p(class = "text-muted",
                                    ui_text("import_intro", lang())))
    })

    output$file_controls <- shiny::renderUI({
      l <- lang()
      shiny::fileInput(ns("file"), ui_text("import_file", l),
                       buttonLabel = ui_text("import_browse", l),
                       placeholder = "", accept = c(".csv", ".tsv", ".txt",
                                                    ".xlsx", ".xls"))
    })

    path <- shiny::reactive({
      f <- input$file
      shiny::req(f)
      # Keep the original extension: it decides how the file is read.
      dest <- paste0(f$datapath, ".", tolower(tools::file_ext(f$name)))
      if (!file.exists(dest)) file.copy(f$datapath, dest)
      dest
    })

    sheets <- shiny::reactive(table_sheets(path()))

    table <- shiny::reactive({
      p <- path()
      sh <- input$sheet
      if (!is.null(sheets()) && is.null(sh)) return(NULL)
      tryCatch(read_table_file(p, sheet = if (is.null(sheets())) NULL else sh),
               error = function(e) e)
    })

    output$options <- shiny::renderUI({
      shiny::req(input$file)
      l <- lang()
      sh <- sheets()
      shiny::tagList(
        if (!is.null(sh)) shiny::selectInput(ns("sheet"), ui_text("import_sheet", l),
                                             choices = sh, selectize = FALSE),
        shiny::tags$div(
          class = "row",
          shiny::tags$div(class = "col-md-6", shiny::selectInput(
            ns("date_format"), ui_text("import_date_format", l),
            choices = stats::setNames(c("iso", "dmy", "mdy"),
                                      c(ui_text("fmt_iso", l), ui_text("fmt_dmy", l),
                                        ui_text("fmt_mdy", l))),
            selectize = FALSE)),
          shiny::tags$div(class = "col-md-6", shiny::selectInput(
            ns("decimal"), ui_text("import_decimal", l),
            choices = stats::setNames(c(".", ","),
                                      c(ui_text("dec_point", l),
                                        ui_text("dec_comma", l))),
            selectize = FALSE))))
    })

    output$mapping <- shiny::renderUI({
      tb <- table()
      shiny::req(tb)
      l <- lang()
      if (inherits(tb, "error")) {
        return(status_alert(ui_text("msg_error", l, conditionMessage(tb)), "danger"))
      }
      dict <- state$reg$dict
      guess <- suggest_mapping(names(tb), dict)
      cols <- c(stats::setNames("", ui_text("import_not_in_file", l)),
                stats::setNames(names(tb), names(tb)))
      sels <- lapply(dict$variables, function(v) {
        shiny::selectInput(ns(paste0("m_", v$name)),
                           sprintf("%s (%s)", v$label[[l]], v$name),
                           choices = cols,
                           selected = if (is.na(guess[[v$name]])) "" else guess[[v$name]],
                           selectize = FALSE, width = "100%")
      })
      shiny::tagList(
        shiny::tags$p(class = "mb-1", shiny::tags$strong(ui_text("import_mapping", l)),
                      .dot, ui_text("import_rows_read", l, nrow(tb))),
        do.call(bslib::layout_column_wrap, c(list(width = "260px"), sels)))
    })

    mapping <- shiny::reactive({
      dict <- state$reg$dict
      m <- vapply(variable_names(dict), function(nm) {
        x <- input[[paste0("m_", nm)]]
        if (is.null(x) || !nzchar(x)) NA_character_ else x
      }, character(1))
      m
    })

    # Any change of file or settings invalidates the previous check.
    shiny::observeEvent(
      list(input$file, input$sheet, input$date_format, input$decimal, mapping()),
      {
        ready(NULL)
        result(NULL)
        issues(NULL)
      }, ignoreInit = TRUE)

    output$actions <- shiny::renderUI({
      shiny::req(input$file)
      l <- lang()
      shiny::tags$div(
        class = "d-flex gap-2 my-2",
        shiny::actionButton(ns("check"), ui_text("import_check", l),
                            class = "btn-outline-primary"),
        if (!is.null(ready())) {
          shiny::actionButton(ns("do_import"), ui_text("import_do", l),
                              class = "btn-primary")
        },
        if (!is.null(issues()) && nrow(issues()) > 0L) {
          shiny::downloadButton(ns("report"), ui_text("import_download_report", l),
                                class = "btn-outline-secondary")
        })
    })

    shiny::observeEvent(lang(), result(NULL), ignoreInit = TRUE)

    output$result <- shiny::renderUI({
      iss <- issues()
      shiny::tagList(
        result(),
        if (!is.null(iss) && nrow(iss) > 0L) {
          shiny::tags$p(class = "text-muted small", ui_text("import_row_note", lang()))
        })
    })

    shiny::observeEvent(input$check, {
      l <- lang()
      reg <- state$reg
      tb <- table()
      ready(NULL)
      if (is.null(tb) || inherits(tb, "error")) {
        result(status_alert(ui_text("import_need_file", l), "warning"))
        return()
      }
      m <- mapping()
      if (all(is.na(m))) {
        result(status_alert(ui_text("import_no_columns", l), "warning"))
        return()
      }
      out <- tryCatch({
        conv <- convert_import_values(apply_mapping(tb, m), reg$dict,
                                      date_format = input$date_format %||% "iso",
                                      decimal_mark = input$decimal %||% ".")
        list(data = conv, issues = registry_check(reg, conv, lang = l))
      }, error = function(e) e)
      if (inherits(out, "error")) {
        result(status_alert(ui_text("msg_error", l, conditionMessage(out)), "danger"))
        issues(NULL)
        return()
      }
      issues(out$issues)
      if (nrow(out$issues) == 0L) {
        ready(out$data)
        result(status_alert(ui_text("import_ready", l, nrow(out$data)), "success"))
      } else {
        result(status_alert(
          ui_text("import_problems", l, nrow(out$issues),
                  length(unique(out$issues$row))), "danger"))
      }
    })

    problems_table <- shiny::reactive({
      iss <- issues()
      shiny::req(iss, nrow(iss) > 0L)
      l <- lang()
      labs <- var_labels(state$reg$dict, l)
      out <- data.frame(
        row = iss$row, record = iss$record_id,
        variable = unname(ifelse(iss$variable %in% names(labs),
                                 labs[iss$variable], iss$variable)),
        message = iss$message, stringsAsFactors = FALSE)
      names(out) <- c(ui_text("import_col_row", l), ui_text("import_col_record", l),
                      ui_text("import_col_variable", l),
                      ui_text("import_col_message", l))
      out
    })

    output$problems <- DT::renderDT(dt_table(problems_table(), lang()))

    output$report <- shiny::downloadHandler(
      filename = function() {
        paste0("registat-import-problems-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) write_csv_excel(problems_table(), file))

    shiny::observeEvent(input$do_import, {
      l <- lang()
      dat <- ready()
      shiny::req(dat)
      if (!nzchar(trimws(user() %||% ""))) {
        result(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      res <- guarded({
        registry_import(state$reg, dat, user = user(), lang = l)
        NULL
      }, state$reg$dict, l)
      if (is.null(res)) {
        result(status_alert(ui_text("import_done", l, nrow(dat)), "success"))
        ready(NULL)
        state$tick <- state$tick + 1L
      } else {
        result(res)
      }
    })
  })
}
