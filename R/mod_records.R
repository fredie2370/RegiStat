# Records tab: table, edit and delete (both with a mandatory reason).

records_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
    shiny::uiOutput(ns("status")),
    DT::DTOutput(ns("table"))
  )
}

records_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- shiny::reactiveVal(NULL)

    data <- shiny::reactive({
      state$tick
      shiny::req(state$reg)
      registry_data(state$reg, include_deleted = isTRUE(input$show_deleted))
    })

    output$controls <- shiny::renderUI({
      l <- lang()
      shiny::tags$div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-2",
        shiny::actionButton(ns("edit"), ui_text("records_edit", l),
                            class = "btn-outline-primary"),
        shiny::actionButton(ns("delete"), ui_text("records_delete", l),
                            class = "btn-outline-danger"),
        shiny::downloadButton(ns("download"), ui_text("records_download", l),
                              class = "btn-outline-secondary"),
        shiny::checkboxInput(ns("show_deleted"), ui_text("records_show_deleted", l),
                             value = isTRUE(shiny::isolate(input$show_deleted))))
    })

    output$status <- shiny::renderUI(status())
    shiny::observeEvent(lang(), status(NULL), ignoreInit = TRUE)

    shown <- shiny::reactive({
      d <- data()
      dict <- state$reg$dict
      labs <- var_labels(dict, lang())
      out <- derive_variables(d, lang())
      for (nm in variable_names(dict)) out[[nm]] <- as.character(out[[nm]])
      names(out)[match(variable_names(dict), names(out))] <- unname(labs)
      dl <- derived_labels(lang())
      hit <- names(out) %in% names(dl)
      for (nm in names(out)[hit]) out[[nm]] <- as.character(out[[nm]])
      names(out)[hit] <- unname(dl[names(out)[hit]])
      if (".deleted" %in% names(out)) {
        out$.deleted <- ifelse(d$.deleted, ui_text("answer_yes", lang()),
                               ui_text("answer_no", lang()))
        names(out)[names(out) == ".deleted"] <- ui_text("records_deleted_col", lang())
      }
      out
    })

    output$table <- DT::renderDT(dt_table(shown(), lang(), selection = "single"))

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("registat-records-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        d <- derive_variables(data(), lang())
        for (nm in names(d)) d[[nm]] <- as.character(d[[nm]])
        write_csv_excel(d, file)
      })

    selected_id <- function() {
      row <- input$table_rows_selected
      if (length(row) != 1L) return(NULL)
      idvar <- id_variable(state$reg$dict)
      as.character(data()[[idvar]][row])
    }

    # ---- edit ---------------------------------------------------------------
    editing <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$edit, {
      status(NULL)
      rid <- selected_id()
      l <- lang()
      if (is.null(rid)) {
        status(status_alert(ui_text("records_select_first", l), "info"))
        return()
      }
      d <- data()
      idvar <- id_variable(state$reg$dict)
      row <- d[d[[idvar]] == rid, , drop = FALSE]
      if (isTRUE(row$.deleted)) return()
      current <- stats::setNames(
        lapply(variable_names(state$reg$dict),
               function(nm) scalar_to_string(row[[nm]], nm)),
        variable_names(state$reg$dict))
      editing(list(id = rid, current = current))
      shiny::showModal(shiny::modalDialog(
        title = ui_text("edit_title", l, rid), size = "xl", easyClose = FALSE,
        shiny::uiOutput(ns("edit_status")),
        form_fields(state$reg$dict, ns, l, prefix = "e_", values = current,
                    skip = idvar),
        shiny::textInput(ns("edit_reason"), ui_text("edit_reason", l),
                         width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton(ui_text("cancel", l)),
          shiny::actionButton(ns("edit_save"), ui_text("edit_save", l),
                              class = "btn-primary"))))
    })

    edit_status <- shiny::reactiveVal(NULL)
    output$edit_status <- shiny::renderUI(edit_status())

    shiny::observeEvent(input$edit_save, {
      info <- editing()
      shiny::req(info)
      reg <- state$reg
      l <- lang()
      idvar <- id_variable(reg$dict)
      if (!nzchar(trimws(user() %||% ""))) {
        edit_status(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      if (!nzchar(trimws(input$edit_reason %||% ""))) {
        edit_status(status_alert(ui_text("reason_required", l), "warning"))
        return()
      }
      new <- form_values(input, reg$dict, prefix = "e_", skip = idvar)
      cur <- info$current[names(new)]
      same <- vapply(names(new), function(nm) {
        identical(is.na(new[[nm]]), is.na(cur[[nm]])) &&
          (is.na(new[[nm]]) || identical(new[[nm]], cur[[nm]]))
      }, logical(1))
      if (all(same)) {
        edit_status(status_alert(ui_text("edit_no_changes", l), "info"))
        return()
      }
      res <- guarded({
        registry_update(reg, info$id, new[!same], user = user(),
                        reason = input$edit_reason, lang = l)
        NULL
      }, reg$dict, l)
      if (is.null(res)) {
        edit_status(NULL)
        shiny::removeModal()
        status(status_alert(ui_text("edit_saved", l, info$id), "success"))
        editing(NULL)
        state$tick <- state$tick + 1L
      } else {
        edit_status(res)
      }
    })

    # ---- delete -------------------------------------------------------------
    deleting <- shiny::reactiveVal(NULL)
    delete_status <- shiny::reactiveVal(NULL)
    output$delete_status <- shiny::renderUI(delete_status())

    shiny::observeEvent(input$delete, {
      status(NULL)
      delete_status(NULL)
      rid <- selected_id()
      l <- lang()
      if (is.null(rid)) {
        status(status_alert(ui_text("records_select_first", l), "info"))
        return()
      }
      deleting(rid)
      shiny::showModal(shiny::modalDialog(
        title = ui_text("delete_title", l, rid), easyClose = FALSE,
        shiny::tags$p(ui_text("delete_explain", l)),
        shiny::uiOutput(ns("delete_status")),
        shiny::textInput(ns("delete_reason"), ui_text("delete_reason", l),
                         width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton(ui_text("cancel", l)),
          shiny::actionButton(ns("delete_confirm"), ui_text("delete_confirm", l),
                              class = "btn-danger"))))
    })

    shiny::observeEvent(input$delete_confirm, {
      rid <- deleting()
      shiny::req(rid)
      reg <- state$reg
      l <- lang()
      if (!nzchar(trimws(user() %||% ""))) {
        delete_status(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      if (!nzchar(trimws(input$delete_reason %||% ""))) {
        delete_status(status_alert(ui_text("reason_required", l), "warning"))
        return()
      }
      res <- guarded({
        registry_delete(reg, rid, user = user(), reason = input$delete_reason)
        NULL
      }, reg$dict, l)
      if (is.null(res)) {
        shiny::removeModal()
        status(status_alert(ui_text("delete_done", l, rid), "success"))
        deleting(NULL)
        state$tick <- state$tick + 1L
      } else {
        delete_status(res)
      }
    })
  })
}
