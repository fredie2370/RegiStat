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

# A table with a CSV download button.
tbl_block <- function(ns, id) {
  shiny::tagList(
    DT::DTOutput(ns(id)),
    shiny::downloadButton(ns(paste0(id, "_csv")), "CSV",
                          class = "btn-outline-secondary btn-sm mt-1 mb-2"))
}

# A static figure with PNG and SVG download buttons. The figure is drawn
# with the same function that exports it, so what is seen is what is saved.
fig_block <- function(ns, id, height = "420px") {
  shiny::tagList(
    shiny::plotOutput(ns(id), height = height),
    shiny::div(class = "mb-3",
               shiny::downloadButton(ns(paste0(id, "_png")), "PNG",
                                     class = "btn-outline-secondary btn-sm"),
               shiny::downloadButton(ns(paste0(id, "_svg")), "SVG",
                                     class = "btn-outline-secondary btn-sm")))
}

# Sensible starting predictors: the usual clinical candidates present in the
# dictionary (anything else can be added by hand).
default_predictors <- function(a) {
  cand <- c("age_years", "sex_at_birth", "systolic_bp_mmhg", "hypertension",
            "fasting_glucose_mgdl", "hemoglobin_gdl", "uric_acid_mgdl", "tyg")
  intersect(cand, names(a$types))
}

default_biomarkers <- function(a) {
  cand <- c("uric_acid_mgdl", "tyg", "fasting_glucose_mgdl", "hemoglobin_gdl", "egfr",
            "creatinine_mgdl", "systolic_bp_mmhg", "bmi")
  intersect(cand, names(a$types)[a$types == "numeric"])
}

# Column names of the result tables, in the language of the interface.
pretty_names <- function(df, lang) {
  map <- c(
    label = "tbl_variable", exposure_label = "col_exposure", outcome_label = "col_outcome",
    compared = "col_compared", n = "col_nobs", test = "col_test", or = "col_or",
    ci = "col_ci", p_value = "col_p", p_adj = "col_p_adj", r = "col_r",
    method = "col_method", events = "col_events", auc = "col_auc", cutoff = "col_cutoff",
    sensitivity = "col_sens_pct", specificity = "col_spec_pct", ppv = "col_ppv",
    npv = "col_npv", interpretation = "col_interp", biomarker_label = "col_biomarker",
    group = "col_group", pct = "col_prev", crude_or = "col_crude_or", crude_p = "col_p",
    adj_or = "col_adj_or", adj_p = "col_p", w = "col_w", normal = "col_normal",
    direction = "col_direction", n_missing = "tbl_missing", crude_ci = "col_ci",
    adj_ci = "col_ci", label1 = "col_exposure", label2 = "col_exposure",
    pct_missing = "col_pct_missing", q1 = "col_q1", q3 = "col_q3", lower = "col_lower",
    upper = "col_upper", n_outliers = "col_n_outliers", rows = "col_rows",
    values = "col_values")
  nm <- names(df)
  hit <- nm %in% names(map)
  nm[hit] <- vapply(nm[hit], function(k) ui_text(map[[k]], lang), character(1))
  names(df) <- nm
  df
}

# Numbers for display: three decimals for p-values, two for the rest.
show_numbers <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (!is.numeric(x)) next
    digits <- if (nm %in% c("auc", "r", "w")) 3L else 2L
    df[[nm]] <- if (grepl("^(p_|p$|crude_p|adj_p)", nm)) vapply(x, fmt_p, character(1)) else
      ifelse(is.na(x), "", ifelse(x == round(x) & abs(x) < 1e6 & digits == 2L, as.character(x),
                                  formatC(x, format = "f", digits = digits)))
  }
  df
}

analysis_server <- function(id, state, lang, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prep <- shiny::reactive({
      state$tick
      shiny::req(state$reg)
      analysis_prepare(registry_data(state$reg), state$reg$dict, lang(),
                       egfr_equation = input$egfr_eq %||% "2021")
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
      bin_ch <- binary_variables(a)
      pred_ch <- choices(a)
      pred_ch <- pred_ch[!(unname(pred_ch) %in% c(
        "n_complications", "n_comorbidities", "n_comp_comorb", "bmi_category",
        "egfr_stage"))]
      bslib::navset_pill(
        bslib::nav_panel(
          ui_text("ana_sub_table", l),
          shiny::tags$div(
            class = "mt-3",
            shiny::tags$div(
              class = "d-flex flex-wrap gap-3 align-items-end",
              shiny::selectInput(ns("by"), ui_text("ana_by", l), choices = cat_ch,
                                 selectize = FALSE, width = "20rem"),
              shiny::selectInput(ns("tstyle"), ui_text("ana_table_style", l),
                                 choices = stats::setNames(
                                   c("both", "auto"),
                                   c(ui_text("ana_style_both", l), ui_text("ana_style_auto", l))),
                                 selectize = FALSE, width = "24rem"),
              shiny::selectInput(ns("egfr_eq"), ui_text("ana_egfr_eq", l),
                                 choices = stats::setNames(
                                   c("2021", "2009"),
                                   c(ui_text("ana_egfr_2021", l), ui_text("ana_egfr_2009", l))),
                                 selected = shiny::isolate(input$egfr_eq) %||% "2021",
                                 selectize = FALSE, width = "22rem"),
              shiny::downloadButton(ns("table_csv"), ui_text("records_download", l),
                                    class = "btn-outline-secondary mb-3")),
            DT::DTOutput(ns("table")),
            shiny::tags$p(class = "text-muted small mt-2", ui_text("rep_note_tests", l)),
            shiny::tags$h5(class = "mt-4", ui_text("ana_quality_title", l)),
            shiny::tags$h6(ui_text("ana_missing_title", l)),
            tbl_block(ns, "miss"),
            shiny::tags$h6(class = "mt-3", ui_text("ana_outlier_title", l)),
            shiny::tags$p(class = "text-muted small", ui_text("ana_outlier_note", l)),
            tbl_block(ns, "outl"))),
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
                                 selectize = FALSE, width = "20rem"),
              shiny::selectInput(ns("smooth"), ui_text("ana_smooth", l),
                                 choices = stats::setNames(
                                   c("lm", "loess"),
                                   c(ui_text("ana_smooth_lm", l), ui_text("ana_smooth_loess", l))),
                                 selectize = FALSE, width = "16rem")),
            plot_output(ns("plot_rel")))),
        bslib::nav_panel(
          ui_text("ana_sub_bivariate", l),
          shiny::tags$div(
            class = "mt-3",
            shiny::tags$p(class = "text-muted small", ui_text("ana_methods_note", l)),
            shiny::tags$h5(ui_text("ana_prev_title", l)),
            tbl_block(ns, "prev"),
            fig_block(ns, "fig_prev", "380px"),
            shiny::tags$h5(class = "mt-4", ui_text("ana_normality_title", l)),
            tbl_block(ns, "norm"),
            shiny::selectInput(ns("norm_var"), ui_text("ana_var", l), choices = num_ch,
                               selectize = FALSE, width = "22rem"),
            fig_block(ns, "fig_norm", "340px"),
            shiny::tags$h5(class = "mt-4", ui_text("ana_cont_title", l)),
            if (length(bin_ch) == 0L) {
              shiny::tags$p(class = "text-muted", ui_text("ana_need_binary", l))
            } else shiny::tagList(
              shiny::tags$div(
                class = "d-flex flex-wrap gap-3",
                shiny::selectInput(ns("bv_out"), ui_text("ana_outcome", l), choices = bin_ch,
                                   selectize = FALSE, width = "22rem"),
                shiny::selectInput(ns("bv_stat"), ui_text("ana_summary_stat", l),
                                   choices = stats::setNames(
                                     c("median", "mean"),
                                     c(ui_text("ana_stat_median", l), ui_text("ana_stat_mean", l))),
                                   selectize = FALSE, width = "20rem")),
              tbl_block(ns, "cont"),
              fig_block(ns, "fig_cmp", "520px"),
              shiny::tags$h5(class = "mt-4", ui_text("ana_cat_title", l)),
              tbl_block(ns, "cat"))),
          shiny::tags$div(
            class = "mt-4",
            shiny::tags$h5(ui_text("ana_corr_title", l)),
            shiny::tags$div(
              class = "d-flex flex-wrap gap-3 align-items-end",
              shiny::selectInput(ns("cor_vars"), ui_text("ana_corr_vars", l), choices = num_ch,
                                 selected = utils::head(unname(num_ch), 8L), multiple = TRUE,
                                 selectize = FALSE, size = 8, width = "24rem"),
              shiny::selectInput(ns("cor_method"), ui_text("ana_corr_method", l),
                                 choices = c(Spearman = "spearman", Pearson = "pearson",
                                             Auto = "auto"),
                                 selectize = FALSE, width = "12rem")),
            tbl_block(ns, "corr"),
            fig_block(ns, "fig_cor", "520px"))),
        bslib::nav_panel(
          ui_text("ana_sub_regression", l),
          shiny::tags$div(
            class = "mt-3",
            if (length(bin_ch) == 0L) {
              shiny::tags$p(class = "text-muted", ui_text("ana_need_binary", l))
            } else shiny::tagList(
              shiny::tags$div(
                class = "d-flex flex-wrap gap-3 align-items-end",
                shiny::selectInput(ns("rg_out"), ui_text("ana_outcomes", l), choices = bin_ch,
                                   selected = utils::head(unname(bin_ch), 1L), multiple = TRUE,
                                   selectize = FALSE, size = 7, width = "22rem"),
                shiny::selectInput(ns("rg_pred"), ui_text("ana_predictors", l),
                                   choices = pred_ch,
                                   selected = default_predictors(a), multiple = TRUE,
                                   selectize = FALSE, size = 10, width = "26rem"),
                shiny::radioButtons(ns("rg_sel"), ui_text("ana_selection", l),
                                    choices = stats::setNames(
                                      c("backward", "none"),
                                      c(ui_text("ana_sel_backward", l), ui_text("ana_sel_none", l)))),
                shiny::actionButton(ns("rg_run"), ui_text("ana_run", l),
                                    class = "btn-primary mb-3")),
              shiny::uiOutput(ns("rg_msg")),
              shiny::tags$h5(ui_text("ana_fit_title", l)),
              tbl_block(ns, "rg_fit"),
              shiny::tags$h5(class = "mt-3", ui_text("ana_or_title", l)),
              tbl_block(ns, "rg_or"),
              fig_block(ns, "fig_forest", "380px"),
              fig_block(ns, "fig_mroc", "420px"),
              shiny::tags$h5(class = "mt-4", ui_text("ana_matrix_title", l)),
              tbl_block(ns, "rg_sum"),
              fig_block(ns, "fig_mat", "420px")))),
        bslib::nav_panel(
          ui_text("ana_sub_roc", l),
          shiny::tags$div(
            class = "mt-3",
            if (length(bin_ch) == 0L) {
              shiny::tags$p(class = "text-muted", ui_text("ana_need_binary", l))
            } else shiny::tagList(
              shiny::tags$div(
                class = "d-flex flex-wrap gap-3 align-items-end",
                shiny::selectInput(ns("rc_out"), ui_text("ana_outcomes", l), choices = bin_ch,
                                   selected = utils::head(unname(bin_ch), 3L), multiple = TRUE,
                                   selectize = FALSE, size = 7, width = "22rem"),
                shiny::selectInput(ns("rc_bio"), ui_text("ana_biomarkers", l), choices = num_ch,
                                   selected = default_biomarkers(a), multiple = TRUE,
                                   selectize = FALSE, size = 10, width = "26rem"),
                shiny::actionButton(ns("rc_run"), ui_text("ana_run", l),
                                    class = "btn-primary mb-3")),
              shiny::uiOutput(ns("rc_msg")),
              shiny::tags$h5(ui_text("ana_auc_title", l)),
              tbl_block(ns, "rc_tab"),
              shiny::tags$h5(class = "mt-3", ui_text("ana_heat_title", l)),
              fig_block(ns, "fig_heat", "460px"),
              shiny::tags$h5(class = "mt-3", ui_text("ana_roc_title", l)),
              shiny::selectInput(ns("rc_show"), ui_text("ana_outcome", l), choices = bin_ch,
                                 selectize = FALSE, width = "22rem"),
              fig_block(ns, "fig_roc", "520px")))),
        bslib::nav_panel(
          ui_text("ana_sub_methods", l),
          shiny::tags$div(
            class = "mt-3",
            lapply(c("meth_intro", "meth_descriptive", "meth_normality", "meth_bivariate",
                     "meth_corr", "meth_regression", "meth_roc"),
                   function(k) shiny::tags$p(ui_text(k, l))))),
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
      t1 <- table_one(a, by = by_or_null(input$by), style = input$tstyle %||% "both")
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
      plot_relation(a, input$x, input$y, color = by_or_null(input$color),
                    smooth = input$smooth %||% "lm")
    })


    # -- helpers to register a table or a figure with its downloads ----------
    tbl_output <- function(id, df_fn) {
      output[[id]] <- DT::renderDT({
        df <- df_fn()
        shiny::validate(shiny::need(is.data.frame(df) && nrow(df) > 0L,
                                    ui_text("ana_few_data", lang())))
        dt_table(pretty_names(show_numbers(df), lang()), lang(), page_length = 15L)
      })
      output[[paste0(id, "_csv")]] <- shiny::downloadHandler(
        filename = function() paste0("registat-", id, "-", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
        content = function(file) write_csv_excel(pretty_names(df_fn(), lang()), file))
    }

    fig_output <- function(id, plot_fn, width = 180, height = 120) {
      output[[id]] <- shiny::renderPlot({
        p <- plot_fn()
        shiny::validate(shiny::need(!is.null(p), ui_text("ana_few_data", lang())))
        p
      }, res = 96)
      for (ext in c("png", "svg")) {
        local({
          e <- ext
          output[[paste0(id, "_", e)]] <- shiny::downloadHandler(
            filename = function() paste0("registat-", id, "-", format(Sys.Date(), "%Y-%m-%d"), ".", e),
            content = function(file) {
              p <- plot_fn()
              shiny::req(p)
              tmp <- tempfile(fileext = paste0(".", e))
              on.exit(unlink(tmp), add = TRUE)
              save_figure(p, tmp, width, height)
              file.copy(tmp, file, overwrite = TRUE)
            })
        })
      }
    }

    # Runs an analysis and turns failures into a friendly message.
    safely <- function(expr) {
      tryCatch(expr, error = function(e) structure(list(msg = conditionMessage(e)), class = "ana_fail"))
    }
    failed <- function(x) inherits(x, "ana_fail")
    fail_alert <- function(x) {
      if (failed(x)) status_alert(ui_text("ana_error", lang(), x$msg), "warning")
    }

    # -- data quality ----------------------------------------------------------
    tbl_output("miss", function() {
      m <- missing_summary(prep())
      m <- m[m$n_missing > 0L, c("label", "n_missing", "pct_missing"), drop = FALSE]
      if (nrow(m) == 0L) NULL else m
    })
    tbl_output("outl", function() {
      o <- outlier_summary(prep())
      o <- o[o$n_outliers > 0L, c("label", "n", "q1", "q3", "lower", "upper", "n_outliers",
                                   "rows", "values"), drop = FALSE]
      if (nrow(o) == 0L) NULL else o
    })

    # -- prevalence, normality, comparisons ----------------------------------
    tbl_output("prev", function() {
      a <- prep()
      p <- safely(prevalence_table(a, by = by_or_null(input$by)))
      if (failed(p)) return(NULL)
      p$ci <- sprintf("%s - %s", fmt_num(p$ci_low, 1L), fmt_num(p$ci_high, 1L))
      p[, c("label", "group", "n", "N", "pct", "ci")]
    })
    fig_output("fig_prev", function() {
      p <- safely(plot_prevalence(prep(), by = by_or_null(input$by)))
      if (failed(p)) NULL else p
    }, 250, 130)

    tbl_output("norm", function() {
      t <- safely(normality_table(prep()))
      if (failed(t)) return(NULL)
      t$normal <- ifelse(is.na(t$normal), "", ifelse(t$normal, ui_text("yes_word", lang()),
                                                      ui_text("no_word", lang())))
      t[, c("label", "n", "w", "p_value", "normal")]
    })
    fig_output("fig_norm", function() {
      a <- prep()
      shiny::req(input$norm_var %in% names(a$types))
      p <- safely(plot_normality(a, input$norm_var))
      if (failed(p)) NULL else p
    }, 150, 100)

    tbl_output("cont", function() {
      shiny::req(input$bv_out)
      t <- safely(compare_continuous(prep(), input$bv_out, summary = input$bv_stat %||% "median"))
      if (failed(t)) NULL else t[, setdiff(names(t), "variable"), drop = FALSE]
    })
    fig_output("fig_cmp", function() {
      a <- prep()
      shiny::req(input$bv_out)
      vars <- setdiff(names(a$types)[a$types == "numeric"],
                      c(input$bv_out, "n_complications", "n_comorbidities", "n_comp_comorb"))
      vars <- utils::head(vars, 6L)
      p <- safely(plot_compare_outcome(a, vars, input$bv_out))
      if (failed(p)) NULL else p
    }, 180, 190)
    tbl_output("cat", function() {
      shiny::req(input$bv_out)
      t <- safely(associations_categorical(prep(), input$bv_out))
      if (failed(t)) return(NULL)
      t$ci <- ifelse(is.na(t$or), "", sprintf("%s - %s", fmt_num(t$ci_low, 2L), fmt_num(t$ci_high, 2L)))
      t[, c("exposure_label", "compared", "n", "test", "or", "ci", "p_value")]
    })

    cor_res <- shiny::reactive({
      shiny::req(length(input$cor_vars) >= 2L)
      safely(correlations(prep(), input$cor_vars, input$cor_method %||% "spearman"))
    })
    tbl_output("corr", function() {
      r <- cor_res()
      if (failed(r)) NULL else r[, c("label1", "label2", "method", "n", "r", "p_value", "p_adj")]
    })
    fig_output("fig_cor", function() {
      shiny::req(length(input$cor_vars) >= 2L)
      m <- input$cor_method %||% "spearman"
      p <- safely(plot_correlation(prep(), input$cor_vars, if (m == "pearson") "pearson" else "spearman"))
      if (failed(p)) NULL else p
    }, 180, 150)

    # -- logistic regression ---------------------------------------------------
    rg <- shiny::eventReactive(input$rg_run, {
      shiny::req(input$rg_out, input$rg_pred)
      a <- prep()
      sel <- input$rg_sel %||% "backward"
      shiny::withProgress(message = ui_text("ana_working", lang()), value = 0.5, {
        list(models = lapply(stats::setNames(input$rg_out, input$rg_out),
                             function(o) safely(logistic_model(a, o, input$rg_pred, sel))),
             matrix = safely(regression_matrix(a, input$rg_out, input$rg_pred, sel)))
      })
    })
    rg_first <- shiny::reactive({
      ok <- Filter(Negate(failed), rg()$models)
      if (length(ok)) ok[[1L]] else NULL
    })
    output$rg_msg <- shiny::renderUI({
      r <- rg()
      msgs <- unlist(lapply(r$models, function(m) {
        if (failed(m)) ui_text("ana_error", lang(), m$msg) else m$notes
      }))
      if (length(msgs)) {
        status_alert(shiny::tags$ul(class = "mb-0", lapply(unique(msgs), shiny::tags$li)), "info")
      }
    })
    tbl_output("rg_fit", function() {
      m <- rg_first()
      if (is.null(m)) return(NULL)
      f <- m$fit
      out <- data.frame(
        a = c(ui_text("fit_n", lang()), ui_text("fit_events", lang()), ui_text("fit_epv", lang()),
              ui_text("fit_aic", lang()), ui_text("fit_nagelkerke", lang()),
              ui_text("fit_hl", lang()), ui_text("fit_auc", lang()), ui_text("fit_vif", lang())),
        b = c(f$n, f$events, if (is.na(m$epv)) "" else fmt_num(m$epv, 1L),
              sprintf("%s / %s", fmt_num(f$aic_null, 1L), fmt_num(f$aic, 1L)),
              if (is.na(f$nagelkerke)) "" else fmt_num(f$nagelkerke, 3L),
              if (is.na(f$hl_p)) "" else fmt_p(f$hl_p),
              if (is.na(f$auc)) "" else sprintf("%s (%s - %s)", fmt_num(f$auc, 3L),
                                                fmt_num(f$auc_low, 3L), fmt_num(f$auc_high, 3L)),
              if (is.null(m$vif)) "" else fmt_num(max(m$vif), 2L)),
        stringsAsFactors = FALSE)
      names(out) <- c(ui_text("col_fit_stat", lang()), ui_text("col_value", lang()))
      out
    })
    tbl_output("rg_or", function() {
      m <- rg_first()
      if (is.null(m)) NULL else model_table(m)[, c("label", "crude_or", "crude_ci", "crude_p",
                                                   "adj_or", "adj_ci", "adj_p")]
    })
    fig_output("fig_forest", function() {
      m <- rg_first()
      if (is.null(m) || nrow(m$terms) == 0L) NULL else plot_forest(m)
    }, 180, 110)
    fig_output("fig_mroc", function() {
      m <- rg_first()
      if (is.null(m) || is.null(m$roc)) NULL else plot_model_roc(m)
    }, 120, 120)
    tbl_output("rg_sum", function() {
      r <- rg()$matrix
      if (failed(r) || nrow(r$summary) == 0L) return(NULL)
      s <- r$summary
      s$ci <- sprintf("%s - %s", fmt_num(s$auc_low, 3L), fmt_num(s$auc_high, 3L))
      s$auc <- round(s$auc, 3)
      s$nagelkerke <- round(s$nagelkerke, 3)
      s$hl_p <- round(s$hl_p, 3)
      s[, c("outcome_label", "n", "events", "selected", "nagelkerke", "hl_p", "auc", "ci")]
    })
    fig_output("fig_mat", function() {
      r <- rg()$matrix
      if (failed(r) || nrow(r$matrix) == 0L) NULL else plot_predictor_matrix(r)
    }, 240, 140)

    # -- ROC -------------------------------------------------------------------
    rc <- shiny::eventReactive(input$rc_run, {
      shiny::req(input$rc_out, input$rc_bio)
      shiny::withProgress(message = ui_text("ana_working", lang()), value = 0.5,
                          safely(roc_biomarkers(prep(), input$rc_out, input$rc_bio)))
    })
    output$rc_msg <- shiny::renderUI({
      r <- rc()
      if (failed(r)) return(fail_alert(r))
      sk <- attr(r, "skipped")
      if (!is.null(sk) && nrow(sk) > 0L) {
        a <- prep()
        status_alert(shiny::tagList(
          shiny::tags$strong(ui_text("ana_skipped", lang())),
          shiny::tags$ul(class = "mb-0", lapply(seq_len(nrow(sk)), function(i) {
            shiny::tags$li(sprintf("%s / %s: %s", a$labels[[sk$outcome[i]]],
                                   a$labels[[sk$biomarker[i]]], sk$reason[i]))
          }))), "info")
      }
    })
    tbl_output("rc_tab", function() {
      r <- rc()
      if (failed(r) || nrow(r) == 0L) return(NULL)
      r$ci <- sprintf("%s - %s", fmt_num(r$ci_low, 3L), fmt_num(r$ci_high, 3L))
      r[, c("outcome_label", "biomarker_label", "n", "events", "auc", "ci", "interpretation",
            "cutoff", "sensitivity", "specificity", "ppv", "npv")]
    })
    fig_output("fig_heat", function() {
      r <- rc()
      if (failed(r) || nrow(r) == 0L) NULL else plot_auc_heatmap(r)
    }, 240, 130)
    shiny::observeEvent(rc(), {
      r <- rc()
      if (!failed(r) && nrow(r) > 0L) {
        ch <- unique(r$outcome)
        shiny::updateSelectInput(session, "rc_show",
                                 choices = stats::setNames(ch, unique(r$outcome_label)))
      }
    })
    fig_output("fig_roc", function() {
      r <- rc()
      if (failed(r) || nrow(r) == 0L || !(input$rc_show %in% r$outcome)) NULL else
        plot_roc(r, input$rc_show)
    }, 160, 150)

    output$report <- shiny::downloadHandler(
      filename = function() paste0("registat-report-", format(Sys.Date(), "%Y-%m-%d"), ".html"),
      content = function(file) {
        shiny::withProgress(message = ui_text("ana_working", lang()), value = 0.5, {
          tmp <- tempfile(fileext = ".html")
          on.exit(unlink(tmp), add = TRUE)
          registry_report(state$reg, tmp, lang = lang(), by = by_or_null(input$rby),
                          egfr_equation = input$egfr_eq %||% "2021")
          file.copy(tmp, file, overwrite = TRUE)
        })
      })
  })
}
