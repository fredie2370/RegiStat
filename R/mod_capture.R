# Capture tab: one form generated from the data dictionary.

capture_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("form")),
    shiny::uiOutput(ns("buttons"))
  )
}

capture_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    nonce <- shiny::reactiveVal(0L)
    status <- shiny::reactiveVal(NULL)

    output$title <- shiny::renderUI({
      shiny::tags$div(
        shiny::tags$h4(ui_text("capture_title", lang())),
        shiny::tags$p(class = "text-muted small",
                      ui_text("required_note", lang()), .dot,
                      ui_text("capture_keys", lang())))
    })

    output$form <- shiny::renderUI({
      nonce()
      shiny::req(state$reg)
      shiny::tags$div(class = "registat-form",
                      form_fields(state$reg$dict, ns, lang()))
    })

    output$buttons <- shiny::renderUI({
      shiny::tags$div(
        class = "d-flex gap-2 mt-2",
        shiny::actionButton(ns("save"), ui_text("capture_save", lang()),
                            class = "btn-primary"),
        shiny::actionButton(ns("clear"), ui_text("capture_clear", lang()),
                            class = "btn-outline-secondary"))
    })

    output$status <- shiny::renderUI(status())
    shiny::observeEvent(lang(), status(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$clear, {
      status(NULL)
      nonce(nonce() + 1L)
    })

    shiny::observeEvent(input$save, {
      reg <- state$reg
      l <- lang()
      if (!nzchar(trimws(user() %||% ""))) {
        status(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      rec <- form_values(input, reg$dict)
      saved <- NULL
      res <- guarded({
        saved <- registry_add(reg, rec, user = user(), lang = l)
        NULL
      }, reg$dict, l)
      if (is.null(res)) {
        status(status_alert(ui_text("capture_saved", l, saved), "success"))
        state$tick <- state$tick + 1L
        nonce(nonce() + 1L)
      } else {
        status(res)
      }
    })
  })
}
