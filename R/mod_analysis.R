# Analysis tab: descriptive table, interactive plots and the HTML report.

analysis_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "body"))
}

report_available <- function() {
  requireNamespace("rmarkdown", quietly = TRUE) &&
    requireNamespace("knitr", quietly = TRUE) && rmarkdown::pandoc_available()
}

# Plot output that is interactive when plotly is installed.
plot_output <- function(id) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::plotlyOutput(id, height = "420px")
  } else {
    shiny::plotOutput(id, height = "420px")
  }
}

render_plot_fn <- function(expr_fn) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::renderPlotly(as_interactive(expr_fn()))
  } else {
    shiny::renderPlot(expr_fn())
  }
}

analysis_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prep <- shiny::reactive({
      state$tick
      shiny::req(state$reg)
      analysis_prepare(registry_data(state$reg), state$reg$dict, lang())
    })

    choices <- function(a, type = NULL, none = FALSE) {
      nm <- names(a$types)
      if (!is.null(type)) nm <- nm[a$types == type]
      ch <- stats::setNames(nm, unname(a$labels[nm]))
      if (none) ch <- c(stats::setNames("", ui_text("ana_none", lang())), ch)
      ch
    }

    output$body <- shiny::renderUI({
      l <- lang()
      a <- prep()
      if (nrow(a$data) == 0L) {
        return(shiny::tags$p(class = "text-muted mt-3", ui_text("ana_empty", l)))
      }
      cat_ch <- choices(a, "categorical", none = TRUE)
      num_ch <- choices(a, "numeric")
      bslib::navset_pill(
        bslib::nav_panel(
          ui_text("ana_sub_table", l),
          shiny::tags$div(
            class = "mt-3",
            shiny::tags$div(
              class = "d-flex flex-wrap gap-3 align-items-end",
              shiny::selectInput(ns("by"), ui_text("ana_by", l), choices = cat_ch,
                                 selectize = FALSE, width = "20rem"),
              shiny::downloadButton(ns("table_csv"), ui_text("records_download", l),
                                    class = "btn-outline-secondary mb-3")),
            DT::DTOutput(ns("table")),
            shiny::tags$p(class = "text-muted small mt-2", ui_text("rep_note_tests", l)))),
        bslib::nav_panel(
          ui_text("ana_sub_plots", l),
          shiny::tags$div(
            class = "mt-3",
            shiny::tags$h5(ui_text("ana_distribution", l)),
            shiny::tags$div(
              class = "d-flex flex-wrap gap-3",
              shiny::selectInput(ns("var"), ui_text("ana_var", l),
                                 choices = choices(a), selectize = FALSE,
                                 selected = if ("age_years" %in% names(a$types)) "age_years",
                                 width = "22rem"),
              shiny::selectInput(ns("pby"), ui_text("ana_by", l), choices = cat_ch,
                                 selectize = FALSE, width = "20rem")),
            plot_output(ns("plot_var")),
            shiny::tags$h5(class = "mt-4", ui_text("ana_relation", l)),
            shiny::tags$div(
              class = "d-flex flex-wrap gap-3",
              shiny::selectInput(ns("x"), ui_text("ana_x", l), choices = num_ch,
                                 selectize = FALSE, width = "20rem"),
              shiny::selectInput(ns("y"), ui_text("ana_y", l), choices = num_ch,
                                 selected = if (length(num_ch) > 1L) num_ch[[2L]] else num_ch[[1L]],
                                 selectize = FALSE, width = "20rem"),
              shiny::selectInput(ns("color"), ui_text("ana_color", l), choices = cat_ch,
                                 selectize = FALSE, width = "20rem")),
            plot_output(ns("plot_rel")))),
        bslib::nav_panel(
          ui_text("ana_sub_report", l),
          shiny::tags$div(
            class = "mt-3",
            shiny::tags$p(ui_text("ana_report_intro", l)),
            shiny::selectInput(ns("rby"), ui_text("ana_by", l), choices = cat_ch,
                               selectize = FALSE, width = "20rem"),
            if (report_available()) {
              shiny::downloadButton(ns("report"), ui_text("ana_report_download", l),
                                    class = "btn-primary")
            } else {
              status_alert(ui_text("ana_report_unavailable", l), "warning")
            })))
    })

    by_or_null <- function(x) if (is.null(x) || !nzchar(x)) NULL else x

    table_df <- shiny::reactive({
      a <- prep()
      t1 <- table_one(a, by = by_or_null(input$by))
      t1
    })

    output$table <- DT::renderDT({
      t1 <- table_df()
      shown <- t1[, setdiff(names(t1), "variable"), drop = FALSE]
      shown$label[duplicated(shown$label)] <- ""
      names(shown)[1:2] <- c(ui_text("tbl_variable", lang()), ui_text("tbl_statistic", lang()))
      dt_table(shown, lang(), page_length = 25L)
    })

    output$table_csv <- shiny::downloadHandler(
      filename = function() paste0("registat-table1-", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) write_csv_excel(table_df(), file))

    output$plot_var <- render_plot_fn(function() {
      a <- prep()
      shiny::req(input$var, input$var %in% names(a$types))
      plot_variable(a, input$var, by = by_or_null(input$pby))
    })

    output$plot_rel <- render_plot_fn(function() {
      a <- prep()
      shiny::req(input$x, input$y)
      plot_relation(a, input$x, input$y, color = by_or_null(input$color))
    })

    output$report <- shiny::downloadHandler(
      filename = function() paste0("registat-report-", format(Sys.Date(), "%Y-%m-%d"), ".html"),
      content = function(file) {
        shiny::withProgress(message = ui_text("ana_working", lang()), value = 0.5, {
          tmp <- tempfile(fileext = ".html")
          on.exit(unlink(tmp), add = TRUE)
          registry_report(state$reg, tmp, lang = lang(), by = by_or_null(input$rby))
          file.copy(tmp, file, overwrite = TRUE)
        })
      })
  })
}
