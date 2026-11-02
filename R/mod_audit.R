# Audit tab: the audit trail and the integrity check. The same module serves
# the registry and the case-finding store (`kind`).

audit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
    shiny::uiOutput(ns("verdict")),
    DT::DTOutput(ns("table"))
  )
}

audit_server <- function(id, state, lang, user, kind = c("registry", "screening")) {
  kind <- match.arg(kind)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    verdict <- shiny::reactiveVal(NULL)

    store <- function() if (kind == "registry") state$reg else state$scr
    tick <- function() if (kind == "registry") state$tick else state$stick
    audit_fn <- if (kind == "registry") registry_audit else screening_audit
    verify_fn <- if (kind == "registry") registry_verify else screening_verify

    output$controls <- shiny::renderUI({
      l <- lang()
      shiny::tagList(
        shiny::tags$h4(ui_text("audit_title", l)),
        shiny::tags$p(class = "text-muted small", ui_text("audit_note", l)),
        shiny::tags$div(
          class = "d-flex flex-wrap gap-2 align-items-end mb-2",
          shiny::textInput(ns("expected_head"), ui_text("audit_expected_head", l),
                           width = "26rem"),
          shiny::actionButton(ns("verify"), ui_text("audit_verify", l),
                              class = "btn-primary mb-3")))
    })

    shiny::observeEvent(input$verify, {
      l <- lang()
      exp <- trimws(input$expected_head %||% "")
      v <- tryCatch(
        verify_fn(store(), expected_head = if (nzchar(exp)) exp else NULL),
        error = function(e) e)
      verdict(if (inherits(v, "error")) {
        status_alert(ui_text("msg_error", l, conditionMessage(v)), "danger")
      } else {
        head_line <- shiny::tags$p(class = "mb-0 small",
                                   ui_text("audit_head", l), " ",
                                   shiny::tags$code(v$head))
        if (v$ok) {
          shiny::tags$div(class = "alert alert-success", role = "alert",
                          shiny::tags$p(ui_text("audit_ok", l, v$n_entries)),
                          head_line)
        } else {
          shiny::tags$div(
            class = "alert alert-danger", role = "alert",
            shiny::tags$p(class = "mb-1", ui_text("audit_failed", l)),
            shiny::tags$ul(lapply(v$problems, shiny::tags$li)), head_line)
        }
      })
    })

    output$verdict <- shiny::renderUI(verdict())

    output$table <- DT::renderDT({
      tick()
      shiny::req(store())
      l <- lang()
      a <- audit_fn(store())
      dict <- if (kind == "registry") store()$dict else NULL
      actions <- audit_action_label(a$action, l)
      if (kind == "screening") {
        actions[a$action == "create"] <- ui_text("audit_scr_created", l)
      }
      out <- data.frame(
        a$seq, a$timestamp, a$user, actions,
        ifelse(is.na(a$record_id), "", a$record_id),
        ifelse(is.na(a$reason), "", a$reason),
        unname(mapply(audit_summary, a$action, a$changes,
                      MoreArgs = list(dict = dict, lang = l))),
        stringsAsFactors = FALSE)
      names(out) <- vapply(c("col_seq", "col_time", "col_user", "col_action",
                             "col_record", "col_reason", "col_summary"),
                           ui_text, character(1), lang = l)
      out <- out[order(-a$seq), , drop = FALSE]
      dt_table(out, l)
    })
  })
}
