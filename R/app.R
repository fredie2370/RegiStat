#' Start the RegiStat application
#'
#' Opens the offline Shiny application in your browser: data capture from a
#' form generated from the study dictionary, a records table with audited
#' edits and deletions, import from CSV/Excel files, and the audit trail with
#' its integrity check. Nothing is sent over the internet.
#'
#' @param folder Folder suggested on the start screen. The registry is stored
#'   in this folder as `registry.sqlite`.
#' @param lang Initial interface language, `"es"` or `"en"`. It can be changed
#'   inside the application.
#' @param launch.browser Open the application in the default browser.
#' @param ... Further arguments passed to [shiny::runApp()], for example
#'   `port`.
#' @return Called for its side effect of running the application.
#' @examples
#' if (interactive()) run_app(folder = tempdir(), lang = "en")
#' @export
run_app <- function(folder = getwd(), lang = c("es", "en"),
                    launch.browser = TRUE, ...) {
  lang <- match.arg(lang)
  shiny::runApp(registat_app(folder = folder, lang = lang),
                launch.browser = launch.browser, host = "127.0.0.1", ...)
}

#' The Shiny app object (internal)
#' @noRd
registat_app <- function(folder = getwd(), lang = "es") {
  shiny::shinyApp(app_ui(lang), app_server(folder, lang))
}

app_theme <- function() {
  bslib::bs_theme(
    version = 5, primary = "#0b6e6e",
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI",
                                       "Roboto", "Helvetica Neue", "Arial",
                                       "sans-serif"))
}

# Enter moves to the next field of the capture form; Enter in the last one
# presses the save button. Plain script, no external resources.
.enter_script <- "
document.addEventListener('keydown', function (e) {
  if (e.key !== 'Enter' || e.shiftKey || e.ctrlKey || e.altKey) return;
  var t = e.target;
  var form = t.closest ? t.closest('.registat-form') : null;
  if (!form || t.tagName === 'TEXTAREA') return;
  var fields = Array.prototype.filter.call(
    form.querySelectorAll('input, select'),
    function (el) { return el.type !== 'hidden' && !el.disabled && el.offsetParent !== null; });
  var i = fields.indexOf(t);
  if (i < 0) return;
  e.preventDefault();
  if (i < fields.length - 1) {
    fields[i + 1].focus();
  } else {
    var btn = document.querySelector('.tab-pane.active button[id$=\"-save\"]') ||
              document.querySelector('button[id$=\"capture-save\"]');
    if (btn) { t.blur(); btn.click(); }
  }
});
"

app_ui <- function(lang) {
  bslib::page_fluid(
    theme = app_theme(),
    title = "RegiStat",
    shiny::tags$head(shiny::tags$script(shiny::HTML(.enter_script))),
    shiny::tags$div(
      class = "d-flex flex-wrap align-items-end gap-3 py-3 border-bottom mb-3",
      shiny::uiOutput("brand"),
      shiny::tags$div(class = "ms-auto"),
      shiny::textInput("user", NULL, placeholder = ui_text("user_placeholder", lang),
                       width = "20rem"),
      shiny::selectInput("lang", ui_text("lang_label", lang),
                         choices = stats::setNames(c("es", "en"), c(paste0("Espa", intToUtf8(0xf1), "ol"), "English")),
                         selected = lang, width = "10rem", selectize = FALSE),
      shiny::uiOutput("close_ui")),
    shiny::uiOutput("main")
  )
}

app_server <- function(folder, lang0) {
  function(input, output, session) {
    state <- shiny::reactiveValues(reg = NULL, tick = 0L, scr = NULL, stick = 0L)
    lang <- shiny::reactive({
      l <- input$lang
      if (is.null(l)) lang0 else l
    })
    user <- shiny::reactive(trimws(input$user %||% ""))

    session$onSessionEnded(function() {
      reg <- shiny::isolate(state$reg)
      if (!is.null(reg)) registry_close(reg)
      scr <- shiny::isolate(state$scr)
      if (!is.null(scr)) screening_close(scr)
    })

    output$brand <- shiny::renderUI({
      l <- lang()
      shiny::tags$div(
        shiny::tags$h3(class = "mb-0 text-primary", ui_text("app_title", l)),
        shiny::tags$small(class = "text-muted", ui_text("app_subtitle", l)))
    })

    output$close_ui <- shiny::renderUI({
      if (is.null(state$reg)) return(NULL)
      shiny::tags$div(
        class = "mb-3",
        shiny::actionButton("close", ui_text("close_registry", lang()),
                            class = "btn-outline-secondary"))
    })

    shiny::observeEvent(lang(), {
      l <- lang()
      shiny::updateTextInput(session, "user",
                             placeholder = ui_text("user_placeholder", l))
      shiny::updateSelectInput(session, "lang", label = ui_text("lang_label", l))
    }, ignoreInit = TRUE)

    output$main <- shiny::renderUI({
      l <- lang()
      if (is.null(state$reg)) return(start_ui(folder, l))
      bslib::navset_tab(
        bslib::nav_panel(ui_text("tab_capture", l), capture_ui("capture")),
        bslib::nav_panel(ui_text("tab_records", l), records_ui("records")),
        bslib::nav_panel(ui_text("tab_import", l), import_ui("import")),
        bslib::nav_panel(ui_text("tab_audit", l), audit_ui("audit")),
        bslib::nav_panel(ui_text("tab_screening", l), screening_ui("screening")))
    })

    capture_server("capture", state, lang, user)
    records_server("records", state, lang, user)
    import_server("import", state, lang, user)
    audit_server("audit", state, lang, user)
    screening_server("screening", state, lang, user)

    shiny::observeEvent(input$close, {
      registry_close(state$reg)
      if (!is.null(state$scr)) screening_close(state$scr)
      state$reg <- NULL
      state$scr <- NULL
      state$tick <- 0L
      state$stick <- 0L
    })

    start_message <- shiny::reactiveVal(NULL)
    output$start_message <- shiny::renderUI(start_message())

    registry_path <- function() {
      f <- trimws(input$folder %||% "")
      if (!nzchar(f)) return(NULL)
      file.path(path.expand(f), "registry.sqlite")
    }

    shiny::observeEvent(input$open, {
      l <- lang()
      p <- registry_path()
      if (is.null(p)) {
        start_message(status_alert(ui_text("msg_folder_empty", l), "warning"))
      } else if (!file.exists(p)) {
        start_message(status_alert(ui_text("msg_no_registry", l), "warning"))
      } else {
        res <- tryCatch(registry_open(p), error = function(e) e)
        if (inherits(res, "error")) {
          start_message(status_alert(ui_text("msg_error", l, conditionMessage(res)),
                                     "danger"))
        } else {
          start_message(NULL)
          state$reg <- res
        }
      }
    })

    shiny::observeEvent(input$create, {
      l <- lang()
      p <- registry_path()
      if (is.null(p)) {
        start_message(status_alert(ui_text("msg_folder_empty", l), "warning"))
        return()
      }
      if (!nzchar(user())) {
        start_message(status_alert(ui_text("msg_user_required", l), "warning"))
        return()
      }
      if (file.exists(p)) {
        start_message(status_alert(ui_text("msg_registry_exists", l), "warning"))
        return()
      }
      res <- tryCatch({
        dict <- if (!is.null(input$dict_file)) {
          read_dictionary(input$dict_file$datapath)
        } else {
          load_template(input$template %||% "t2d")
        }
        dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
        registry_create(p, dict, user = user())
      }, error = function(e) e)
      if (inherits(res, "error")) {
        start_message(status_alert(ui_text("msg_error", l, conditionMessage(res)),
                                   "danger"))
      } else {
        start_message(NULL)
        state$reg <- res
      }
    })
  }
}

start_ui <- function(folder, lang) {
  shiny::tags$div(
    class = "mx-auto", style = "max-width: 40rem;",
    shiny::tags$h4(ui_text("start_title", lang)),
    shiny::tags$p(class = "text-muted", ui_text("start_intro", lang)),
    shiny::textInput("folder", ui_text("start_folder", lang),
                     value = shiny::isolate(folder), width = "100%"),
    shiny::selectInput("template", ui_text("start_template", lang),
                       choices = list_templates(), width = "100%",
                       selectize = FALSE),
    shiny::fileInput("dict_file", ui_text("start_dict_file", lang),
                     buttonLabel = ui_text("import_browse", lang),
                     placeholder = "", accept = c(".yml", ".yaml"), width = "100%"),
    shiny::uiOutput("start_message"),
    shiny::tags$div(
      class = "d-flex gap-2",
      shiny::actionButton("open", ui_text("start_open", lang),
                          class = "btn-outline-primary"),
      shiny::actionButton("create", ui_text("start_create", lang),
                          class = "btn-primary")))
}
