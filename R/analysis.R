# Descriptive analysis: analysis-ready data, "Table 1", missing-data summary
# and plots. Everything here works on plain data frames, so it can be used
# from scripts, from the Shiny app and from the report.

.palette <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9",
              "#D55E00", "#F0E442", "#999999")

#' Prepare registry data for analysis
#'
#' Adds the calculated measures ([derive_variables()]), turns categorical
#' variables into factors labelled in the chosen language (levels in the order
#' of the dictionary), and records which variables are numeric or categorical
#' and how they are labelled. Identifier, date and free-text variables are
#' left out of the analysis variables.
#'
#' @param data A data frame, for example from [registry_data()].
#' @param dict A `registat_dictionary`.
#' @param lang `"en"` or `"es"`.
#' @return An object of class `registat_analysis`: a list with `data`,
#'   `types` (named character: `"numeric"` or `"categorical"`), `labels`
#'   (named character) and `lang`.
#' @examples
#' dict <- load_template("t2d")
#' a <- analysis_prepare(synthetic_t2d(60, seed = 1)$data, dict, "en")
#' head(names(a$types))
#' @export
analysis_prepare <- function(data, dict, lang = c("en", "es")) {
  lang <- match.arg(lang)
  d <- derive_variables(data, lang)
  types <- character()
  labels <- character()
  for (v in dict$variables) {
    nm <- v$name
    if (!(nm %in% names(d))) next
    if (v$type %in% c("integer", "numeric")) {
      d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
      types[nm] <- "numeric"
    } else if (identical(v$type, "categorical")) {
      codes <- vapply(v$choices, function(ch) ch$value, character(1))
      labs <- vapply(v$choices, function(ch) ch$label[[lang]] %||% ch$label[["en"]],
                     character(1))
      d[[nm]] <- factor(as.character(d[[nm]]), levels = codes, labels = labs)
      types[nm] <- "categorical"
    } else {
      next
    }
    labels[nm] <- v$label[[lang]] %||% v$label[["en"]]
  }
  dl <- derived_labels(lang)
  for (nm in intersect(names(dl), names(d))) {
    types[nm] <- if (is.factor(d[[nm]])) "categorical" else "numeric"
    labels[nm] <- dl[[nm]]
  }
  structure(list(data = d, types = types, labels = labels, lang = lang),
            class = "registat_analysis")
}

#' @export
print.registat_analysis <- function(x, ...) {
  cat(sprintf("<RegiStat analysis data: %d records, %d variables (%d numeric, %d categorical)>\n",
              nrow(x$data), length(x$types), sum(x$types == "numeric"),
              sum(x$types == "categorical")))
  invisible(x)
}

#' @noRd
check_analysis <- function(a) {
  if (!inherits(a, "registat_analysis")) {
    abort("'a' must come from analysis_prepare().")
  }
  invisible(a)
}

#' @noRd
check_var <- function(a, var, type = NULL) {
  if (!is.character(var) || length(var) != 1L || !(var %in% names(a$types))) {
    abort("Unknown analysis variable: %s", paste(var, collapse = ", "))
  }
  if (!is.null(type) && a$types[[var]] != type) {
    abort("Variable '%s' must be %s.", var, type)
  }
  invisible(var)
}

#' @noRd
fmt_num <- function(x, digits) formatC(round(x, digits), format = "f", digits = digits)

#' @noRd
fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
}

#' @noRd
group_p_value <- function(x, g, type) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- droplevels(g[ok])
  if (nlevels(g) < 2L || length(x) < 2L) return(NA_real_)
  tryCatch({
    if (type == "numeric") {
      if (nlevels(g) == 2L) stats::wilcox.test(x ~ g, exact = FALSE)$p.value
      else stats::kruskal.test(x ~ g)$p.value
    } else {
      tab <- table(droplevels(x), g)
      if (nrow(tab) < 2L || ncol(tab) < 2L) return(NA_real_)
      small <- any(suppressWarnings(stats::chisq.test(tab)$expected) < 5)
      if ((nrow(tab) == 2L && ncol(tab) == 2L) || small) {
        tryCatch(stats::fisher.test(tab)$p.value,
                 error = function(e) {
                   withr::with_seed(1L, stats::chisq.test(
                     tab, simulate.p.value = TRUE, B = 5000L)$p.value)
                 })
      } else {
        stats::chisq.test(tab)$p.value
      }
    }
  }, error = function(e) NA_real_)
}

#' Descriptive table ("Table 1")
#'
#' Summarises each variable overall and, optionally, by the levels of a
#' categorical variable. Numeric variables show mean (SD) and median
#' \[Q1, Q3\]; categorical variables show n (%) of the non-missing values. The
#' number of missing values is reported for variables that have any.
#'
#' With `by`, a p-value compares the groups: Wilcoxon rank-sum test (two
#' groups) or Kruskal-Wallis test (more groups) for numeric variables;
#' Fisher's exact test for 2x2 tables or small expected counts, otherwise the
#' chi-squared test, for categorical variables. These p-values are descriptive
#' aids: no adjustment for multiple comparisons is made, and they do not
#' replace a planned analysis.
#'
#' @param a An object from [analysis_prepare()].
#' @param by Name of a categorical variable to compare groups, or `NULL`.
#' @param vars Variables to include (default: all analysis variables except
#'   `by`).
#' @param digits Decimals for numeric summaries.
#' @return A data frame with `variable`, `label`, `statistic`, `Overall`, one
#'   column per group and `p` (only when `by` is given).
#' @examples
#' dict <- load_template("t2d")
#' a <- analysis_prepare(synthetic_t2d(80, seed = 1)$data, dict, "en")
#' table_one(a, by = "sex_at_birth", vars = c("age_years", "hypertension"))
#' @export
table_one <- function(a, by = NULL, vars = NULL, digits = 1L) {
  check_analysis(a)
  lang <- a$lang
  d <- a$data
  if (!is.null(by)) check_var(a, by, "categorical")
  vars <- if (is.null(vars)) setdiff(names(a$types), by) else vars
  for (v in vars) check_var(a, v)
  vars <- setdiff(vars, by)

  g <- if (is.null(by)) NULL else droplevels(d[[by]])
  groups <- if (is.null(g)) character() else levels(g)
  cols <- function(f) {
    out <- list(Overall = f(rep(TRUE, nrow(d))))
    for (lv in groups) out[[lv]] <- f(!is.na(g) & g == lv)
    out
  }
  head_n <- function(idx) sprintf("(n=%d)", sum(idx))

  rows <- list()
  add <- function(v, stat, values, p = NA_real_) {
    r <- data.frame(variable = v, label = a$labels[[v]], statistic = stat,
                    stringsAsFactors = FALSE)
    for (k in names(values)) r[[k]] <- values[[k]]
    if (!is.null(by)) r$p <- p
    rows[[length(rows) + 1L]] <<- r
  }
  na_stat <- ui_text("tbl_missing", lang)

  for (v in vars) {
    x <- d[[v]]
    p <- if (is.null(by)) NA_real_ else group_p_value(x, g, a$types[[v]])
    miss <- cols(function(idx) sum(is.na(x[idx])))
    if (a$types[[v]] == "numeric") {
      add(v, ui_text("tbl_mean_sd", lang), cols(function(idx) {
        y <- x[idx & !is.na(x)]
        if (length(y) == 0L) "" else
          sprintf("%s (%s)", fmt_num(mean(y), digits),
                  if (length(y) > 1L) fmt_num(stats::sd(y), digits) else "")
      }), p)
      add(v, ui_text("tbl_median_iqr", lang), cols(function(idx) {
        y <- x[idx & !is.na(x)]
        if (length(y) == 0L) "" else {
          q <- stats::quantile(y, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
          sprintf("%s [%s, %s]", fmt_num(q[2], digits), fmt_num(q[1], digits),
                  fmt_num(q[3], digits))
        }
      }))
    } else {
      for (lv in levels(x)) {
        add(v, lv, cols(function(idx) {
          den <- sum(idx & !is.na(x))
          n <- sum(idx & !is.na(x) & x == lv)
          if (den == 0L) "" else sprintf("%d (%s%%)", n, fmt_num(100 * n / den, digits))
        }), if (lv == levels(x)[1L]) p else NA_real_)
      }
    }
    if (any(unlist(miss) > 0L)) add(v, na_stat, lapply(miss, as.character))
  }
  out <- do.call(rbind, rows)
  names(out)[names(out) == "Overall"] <- paste(ui_text("tbl_overall", lang),
                                              head_n(rep(TRUE, nrow(d))))
  for (lv in groups) {
    names(out)[names(out) == lv] <- paste(lv, head_n(!is.na(g) & g == lv))
  }
  if (!is.null(by)) out$p <- fmt_p(out$p)
  rownames(out) <- NULL
  out
}

#' Missing data by variable
#'
#' @param a An object from [analysis_prepare()].
#' @return A data frame with `variable`, `label`, `n_missing`, `pct_missing`
#'   and `n_total`, most missing first.
#' @export
missing_summary <- function(a) {
  check_analysis(a)
  d <- a$data
  n_miss <- vapply(names(a$types), function(v) sum(is.na(d[[v]])), integer(1))
  out <- data.frame(variable = names(a$types), label = unname(a$labels[names(a$types)]),
                    n_missing = unname(n_miss),
                    pct_missing = round(100 * unname(n_miss) / nrow(d), 1),
                    n_total = nrow(d), stringsAsFactors = FALSE)
  out <- out[order(-out$n_missing, out$variable), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- plots ------------------------------------------------------------------

#' @noRd
plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   plot.title = ggplot2::element_text(face = "bold"))
}

#' Plot one variable, optionally by group
#'
#' Numeric variables give a histogram (or, with `by`, box plots with the
#' individual points); categorical variables give bar charts of counts.
#' Missing values are dropped and the plot says how many.
#'
#' @param a An object from [analysis_prepare()].
#' @param var Variable to plot.
#' @param by Optional categorical grouping variable.
#' @return A `ggplot` object. Use [as_interactive()] for a plotly version.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(80, seed = 1)$data, load_template("t2d"), "en")
#' plot_variable(a, "hba1c_pct")
#' plot_variable(a, "hba1c_pct", by = "sex_at_birth")
#' @export
plot_variable <- function(a, var, by = NULL) {
  check_analysis(a)
  check_var(a, var)
  if (!is.null(by)) check_var(a, by, "categorical")
  d <- a$data
  lab <- a$labels[[var]]
  keep <- !is.na(d[[var]]) & (is.null(by) | !is.na(if (is.null(by)) TRUE else d[[by]]))
  n_drop <- sum(!keep)
  d <- d[keep, , drop = FALSE]
  cap <- if (n_drop > 0L) ui_text("plot_dropped", a$lang, n_drop) else NULL

  p <- ggplot2::ggplot(d)
  if (a$types[[var]] == "numeric") {
    if (is.null(by)) {
      p <- p + ggplot2::geom_histogram(ggplot2::aes(x = .data[[var]]), bins = 25,
                                       fill = .palette[1L], colour = "white") +
        ggplot2::labs(x = lab, y = ui_text("plot_count", a$lang))
    } else {
      p <- p + ggplot2::geom_boxplot(
        ggplot2::aes(x = .data[[by]], y = .data[[var]], colour = .data[[by]]),
        outlier.shape = NA, fill = NA) +
        ggplot2::geom_jitter(ggplot2::aes(x = .data[[by]], y = .data[[var]],
                                          colour = .data[[by]]),
                             width = 0.15, alpha = 0.5, size = 1.2) +
        ggplot2::labs(x = a$labels[[by]], y = lab) +
        ggplot2::guides(colour = "none")
    }
  } else {
    if (is.null(by)) {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x = .data[[var]]), fill = .palette[1L]) +
        ggplot2::labs(x = lab, y = ui_text("plot_count", a$lang))
    } else {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x = .data[[var]], fill = .data[[by]]),
                                 position = "dodge") +
        ggplot2::labs(x = lab, y = ui_text("plot_count", a$lang),
                      fill = a$labels[[by]])
    }
  }
  p + ggplot2::scale_colour_manual(values = .palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = .palette, drop = FALSE) +
    ggplot2::labs(title = lab, caption = cap) + plot_theme()
}

#' Scatter plot of two numeric variables
#'
#' Points with a linear trend line, optionally coloured by a categorical
#' variable. The line is a descriptive aid, not a fitted model report.
#'
#' @param a An object from [analysis_prepare()].
#' @param x,y Numeric variables.
#' @param color Optional categorical variable.
#' @return A `ggplot` object.
#' @export
plot_relation <- function(a, x, y, color = NULL) {
  check_analysis(a)
  check_var(a, x, "numeric")
  check_var(a, y, "numeric")
  if (!is.null(color)) check_var(a, color, "categorical")
  d <- a$data
  keep <- !is.na(d[[x]]) & !is.na(d[[y]]) &
    (is.null(color) | !is.na(if (is.null(color)) TRUE else d[[color]]))
  n_drop <- sum(!keep)
  d <- d[keep, , drop = FALSE]
  p <- if (is.null(color)) {
    ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(alpha = 0.6, colour = .palette[1L]) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                           colour = .palette[2L], fill = .palette[2L], alpha = 0.15)
  } else {
    ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                    colour = .data[[color]], fill = .data[[color]])) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
  }
  p + ggplot2::scale_colour_manual(values = .palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = .palette, drop = FALSE) +
    ggplot2::labs(x = a$labels[[x]], y = a$labels[[y]],
                  colour = if (!is.null(color)) a$labels[[color]],
                  fill = if (!is.null(color)) a$labels[[color]],
                  caption = if (n_drop > 0L) ui_text("plot_dropped", a$lang, n_drop)) +
    plot_theme()
}

#' Interactive version of a plot
#'
#' Converts a `ggplot` into a plotly widget (hover, zoom, download as PNG)
#' when the plotly package is installed; otherwise returns the plot
#' unchanged.
#'
#' @param p A `ggplot` object.
#' @return A plotly widget or the original `ggplot`.
#' @export
as_interactive <- function(p) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::config(plotly::ggplotly(p), displaylogo = FALSE)
  } else {
    p
  }
}
