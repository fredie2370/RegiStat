# Publication-style figures for the statistical analyses. All functions return
# ggplot objects with a plain white background, so they can be saved as PNG,
# SVG or PDF with save_figure().

#' @noRd
theme_registat <- function(base_size = 12) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(colour = "gray90", linetype = "dashed"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size),
      plot.subtitle = ggplot2::element_text(colour = "gray40", size = base_size - 2),
      plot.caption = ggplot2::element_text(colour = "gray50", size = base_size - 4),
      legend.position = "top")
}

.sex_colours <- c(female = "#FFC5D3", male = "#92C5FC", intersex = "#CBB8E8",
                  unknown = "#D9D9D9")
.overall_colour <- "#88E788"
.roc_palette <- c("#B07AA1", "#4E79A7", "#F28E2B", "#59A14F", "#E15759",
                  "#EDC948", "#A18262", "#FF9DA7", "#76B7B2", "#9C755F",
                  "#BAB0AC", "#D37295")
.auc_colours <- c(outstanding = "#D4EDDA", excellent = "#CCE5FF",
                  acceptable = "#FFF3CD", unsatisfactory = "#F8D7DA",
                  none = "#E0E0E0")

#' Fill colours for groups (internal)
#'
#' The overall group is green; the levels of sex at birth are pink (female)
#' and blue (male) as in the figures of the thesis; any other grouping uses
#' the colour-blind-safe palette.
#' @noRd
group_colours <- function(a, by, groups) {
  overall <- ui_text("tbl_overall", a$lang)
  cols <- character()
  k <- 0L
  for (g in groups) {
    if (identical(g, overall)) {
      cols[g] <- .overall_colour
      next
    }
    code <- if (!is.null(by)) names(a$codes[[by]])[match(g, a$codes[[by]])] else NA_character_
    if (!is.na(code) && code %in% names(.sex_colours) && identical(by, "sex_at_birth")) {
      cols[g] <- .sex_colours[[code]]
    } else {
      k <- k + 1L
      cols[g] <- .palette[(k - 1L) %% length(.palette) + 1L]
    }
  }
  cols
}

#' Legend text of a group (internal)
#'
#' For sex at birth the levels are written as in the thesis figures
#' ("Assigned female at birth"); other groupings keep their own labels.
#' @noRd
fig_group_label <- function(a, by, z) {
  if (is.null(by) || !identical(by, "sex_at_birth")) return(z)
  cd <- a$codes[[by]]
  code <- names(cd)[match(z, cd)]
  key <- ifelse(code %in% c("female", "male"), paste0("grp_", code), NA_character_)
  out <- z
  ok <- !is.na(key)
  out[ok] <- vapply(key[ok], function(k) ui_text(k, a$lang), character(1))
  out
}

#' Age-by-sex population pyramid
#'
#' Horizontal bars of the number of patients per age group, one side per
#' sex at birth (first level of the grouping variable to the right, second
#' to the left; other levels are not drawn). Counts are written at the end
#' of the bars.
#'
#' @param a An object from [analysis_prepare()].
#' @param age Numeric age variable.
#' @param by Categorical variable with two main levels.
#' @param breaks Age cut points. Groups are closed on the left.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' plot_pyramid(a)
#' @export
plot_pyramid <- function(a, age = "age_years", by = "sex_at_birth",
                         breaks = c(0, 30, 40, 50, 60, 70, 80, Inf)) {
  check_analysis(a)
  check_var(a, age, "numeric")
  check_var(a, by, "categorical")
  g <- clean_factor(a, by)
  lv <- levels(g)[levels(g) %in% names(table(g))[table(g) > 0]]
  if (length(lv) < 2L) abort("The grouping variable needs two levels with records.")
  lv <- lv[1:2]
  ge <- cut(a$data[[age]], breaks = breaks, right = FALSE)
  k <- length(levels(ge))
  labs <- vapply(seq_len(k), function(i) {
    lo <- breaks[i]; hi <- breaks[i + 1L]
    if (i == 1L && is.finite(hi)) sprintf("<%d", hi)
    else if (!is.finite(hi)) sprintf("\u2265%d", lo)
    else sprintf("%d-%d", lo, hi - 1L)
  }, character(1))
  levels(ge) <- labs
  d <- data.frame(group = ge, sex = g)
  d <- d[!is.na(d$group) & d$sex %in% lv, , drop = FALSE]
  cnt <- as.data.frame(table(group = d$group, sex = factor(d$sex, levels = lv)),
                       responseName = "n")
  cnt$n_plot <- ifelse(cnt$sex == lv[2L], -cnt$n, cnt$n)
  mx <- max(cnt$n, 1L)
  cols <- group_colours(a, by, lv)
  ggplot2::ggplot(cnt, ggplot2::aes(x = .data$group, y = .data$n_plot, fill = .data$sex)) +
    ggplot2::geom_col(width = 0.75, alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n,
                                    hjust = ifelse(.data$sex == lv[2L], 1.3, -0.3)),
                       size = 3.2, colour = "gray30") +
    ggplot2::scale_y_continuous(labels = function(x) abs(x),
                                limits = c(-mx * 1.2, mx * 1.2)) +
    ggplot2::scale_fill_manual(values = cols, labels = function(z) fig_group_label(a, by, z)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = ui_text("fig_age_group", a$lang),
                  y = ui_text("fig_n_patients", a$lang), fill = NULL) +
    theme_registat() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "gray90",
                                                              linetype = "dashed"),
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Counts of a categorical variable, overall and by group
#'
#' Grouped bars of the number of patients in each category for everyone and
#' for each level of `by`, labelled with the count and the percentage
#' within the group. The legend shows the group sizes.
#'
#' @param a An object from [analysis_prepare()].
#' @param var Categorical variable (for example BMI category).
#' @param by Optional categorical grouping variable.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' plot_group_counts(a, "bmi_category", by = "sex_at_birth")
#' @export
plot_group_counts <- function(a, var, by = NULL) {
  check_analysis(a)
  check_var(a, var, "categorical")
  if (!is.null(by)) check_var(a, by, "categorical")
  x <- droplevels(a$data[[var]])
  lv <- levels(a$data[[var]])
  overall <- ui_text("tbl_overall", a$lang)
  g <- if (is.null(by)) NULL else clean_factor(a, by)
  groups <- c(overall, if (!is.null(g)) levels(g))
  parts <- lapply(groups, function(k) {
    idx <- if (k == overall) !is.na(x) else !is.na(x) & !is.na(g) & g == k
    t <- table(factor(x[idx], levels = lv))
    data.frame(category = factor(names(t), levels = lv), group = k,
               n = as.integer(t), N = sum(t), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, parts)
  d$pct <- ifelse(d$N > 0L, round(100 * d$n / d$N, 1), 0)
  d$group <- factor(d$group, levels = groups)
  nn <- tapply(d$N, d$group, function(z) z[1L])
  cols <- group_colours(a, by, groups)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$category, y = .data$n, fill = .data$group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data$n, "\n(", .data$pct, "%)")),
                       position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.3, size = 2.7, lineheight = 0.95, colour = "grey30") +
    ggplot2::scale_fill_manual(values = cols,
                               labels = function(z) sprintf("%s (n=%d)",
                                                            fig_group_label(a, by, z), nn[z])) +
    ggplot2::expand_limits(y = max(d$n) * 1.2) +
    ggplot2::labs(x = a$labels[[var]], y = ui_text("fig_n_patients", a$lang), fill = NULL) +
    theme_registat()
}

#' Prevalence of yes/no variables, overall and by group
#'
#' Grouped bars of the number of patients with each complication or
#' comorbidity, for everyone and for each level of `by`, labelled with the
#' count and the percentage of patients with an answer.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Yes/no variables (default: the dictionary's yes/no variables).
#' @param by Optional categorical grouping variable.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' plot_prevalence(a, by = "sex_at_birth")
#' @export
plot_prevalence <- function(a, vars = NULL, by = NULL) {
  check_analysis(a)
  vars <- vars %||% setdiff(names(a$binary), aggregate_vars())
  tab <- prevalence_table(a, vars, by)
  tab$label <- factor(tab$label, levels = unique(a$labels[vars]))
  overall <- ui_text("tbl_overall", a$lang)
  groups <- levels(tab$group)
  g <- if (is.null(by)) NULL else clean_factor(a, by)
  sizes <- c(nrow(a$data), if (!is.null(g)) as.integer(table(g)[levels(g)]))
  names(sizes) <- groups
  cols <- group_colours(a, by, groups)
  ggplot2::ggplot(tab, ggplot2::aes(x = .data$label, y = .data$n, fill = .data$group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7,
                      alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data$n, "\n(", round(.data$pct, 1), "%)")),
                       position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.5, size = 3.2, colour = "gray30") +
    ggplot2::scale_fill_manual(values = cols,
                               labels = function(z) sprintf("%s (n=%d)",
                                                            fig_group_label(a, by, z), sizes[z])) +
    ggplot2::expand_limits(y = max(tab$n) * 1.25) +
    ggplot2::scale_x_discrete(labels = function(z) vapply(strsplit(z, " "), function(w) {
      paste(w, collapse = "\n") }, character(1))) +
    ggplot2::labs(x = ui_text("fig_comp_comorb", a$lang),
                  y = ui_text("fig_n_patients", a$lang), fill = NULL) +
    theme_registat()
}

#' Normality check of a variable
#'
#' Histogram with the empirical density (red) and the normal curve with the
#' same mean and standard deviation (dashed), and the Shapiro-Wilk result
#' in the subtitle.
#'
#' @param a An object from [analysis_prepare()].
#' @param var Numeric variable.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' plot_normality(a, "age_years")
#' @export
plot_normality <- function(a, var) {
  check_analysis(a)
  check_var(a, var, "numeric")
  x <- a$data[[var]]
  x <- x[!is.na(x)]
  if (length(x) < 3L) abort("At least 3 values are needed.")
  nt <- normality_table(a, var)
  sub <- if (is.na(nt$p_value)) "" else
    ui_text(if (nt$normal) "fig_normal_yes" else "fig_normal_no", a$lang, nt$p_value)
  ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = .data$x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(.data$density)), bins = 20,
                            fill = "#4E79A7", colour = "white", alpha = 0.85) +
    ggplot2::geom_density(colour = "#E15759", linewidth = 1) +
    ggplot2::stat_function(fun = stats::dnorm,
                           args = list(mean = mean(x), sd = stats::sd(x)),
                           colour = "black", linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(title = a$labels[[var]], subtitle = sub, x = a$labels[[var]],
                  y = ui_text("fig_density", a$lang),
                  caption = ui_text("fig_normal_caption", a$lang)) +
    theme_registat(11)
}

#' Numeric variables by outcome, with the test result
#'
#' One panel per variable with box plots and points for patients without and
#' with the outcome and the significance of the comparison
#' ([compare_continuous()]: Welch t-test for approximately normal variables,
#' otherwise Wilcoxon). Stars: ns p >= 0.05, * < 0.05, ** < 0.01, *** < 0.001.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Numeric variables.
#' @param outcome A yes/no variable.
#' @param ncol Number of panel columns.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' plot_compare_outcome(a, c("age_years", "creatinine_mgdl"), "ckd")
#' @export
plot_compare_outcome <- function(a, vars, outcome, ncol = 2L) {
  check_analysis(a)
  y <- binary_outcome(a, outcome)
  b <- a$binary[[outcome]]
  tab <- compare_continuous(a, outcome, vars)
  stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                                    ifelse(p < 0.05, "*", "ns"))))
  long <- do.call(rbind, lapply(vars, function(v) {
    d <- data.frame(label = a$labels[[v]], value = a$data[[v]],
                    group = factor(y, levels = c(0L, 1L),
                                   labels = c(b[["negative"]], b[["positive"]])))
    d[!is.na(d$value) & !is.na(d$group), , drop = FALSE]
  }))
  long$label <- factor(long$label, levels = a$labels[vars])
  ann <- data.frame(label = factor(a$labels[tab$variable], levels = a$labels[vars]),
                    text = stars(tab$p_value))
  cols <- stats::setNames(c("#4E79A7", "#E15759"), c(b[["negative"]], b[["positive"]]))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$group, y = .data$value, fill = .data$group)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = 1.5, y = Inf, label = .data$text),
                       inherit.aes = FALSE, vjust = 1.4, size = 4) +
    ggplot2::facet_wrap(~label, scales = "free_y", ncol = ncol) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::labs(x = a$labels[[outcome]], y = NULL,
                  caption = ui_text("fig_compare_caption", a$lang)) +
    theme_registat(10) +
    ggplot2::theme(legend.position = "none",
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
}

#' Correlation heat map
#'
#' Upper triangle of the correlation matrix, variables ordered by
#' hierarchical clustering, with the coefficient written in each cell.
#' Coefficients use all records that have both values.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Numeric variables (at least two).
#' @param method `"spearman"` (default) or `"pearson"`.
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' plot_correlation(a, c("age_years", "hba1c_pct", "fasting_glucose_mgdl", "egfr"))
#' @export
plot_correlation <- function(a, vars, method = c("spearman", "pearson")) {
  check_analysis(a)
  method <- match.arg(method)
  vars <- unique(vars)
  if (length(vars) < 2L) abort("Choose at least two numeric variables.")
  for (v in vars) check_var(a, v, "numeric")
  m <- suppressWarnings(stats::cor(a$data[, vars, drop = FALSE], method = method,
                                   use = "pairwise.complete.obs"))
  m0 <- m
  m0[is.na(m0)] <- 0
  ord <- stats::hclust(stats::as.dist(1 - m0))$order
  vars <- vars[ord]
  m <- m[vars, vars]
  lab <- unname(a$labels[vars])
  d <- expand.grid(i = seq_along(vars), j = seq_along(vars))
  d <- d[d$j > d$i, , drop = FALSE]
  d$r <- m[cbind(d$i, d$j)]
  d$x <- factor(lab[d$j], levels = lab)
  d$y <- factor(lab[d$i], levels = rev(lab))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(.data$r), "", sprintf("%.2f", .data$r))),
                       size = 3) +
    ggplot2::scale_fill_gradient2(low = "#E15759", mid = "white", high = "#4E79A7",
                                  midpoint = 0, limits = c(-1, 1), na.value = "grey90",
                                  name = ui_text(paste0("fig_cor_", method), a$lang)) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 0),
                   panel.grid = ggplot2::element_blank(), legend.position = "right")
}

#' Forest plot of the adjusted odds ratios of a model
#'
#' Odds ratios with their 95% interval on a logarithmic axis; the colour
#' marks p < 0.05. Odds ratios of numeric predictors are per one unit.
#'
#' @param model An object from [logistic_model()].
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' plot_forest(logistic_model(a, "ckd", c("age_years", "hba1c_pct"), selection = "none"))
#' @export
plot_forest <- function(model) {
  if (!inherits(model, "registat_model")) abort("'model' must come from logistic_model().")
  if (nrow(model$terms) == 0L) abort("The model has no predictors to plot.")
  lang <- model$lang
  d <- model$terms
  d$sig <- d$p_value < 0.05
  d$label <- stats::reorder(d$label, d$or)
  f <- model$fit
  ggplot2::ggplot(d, ggplot2::aes(x = .data$or, y = .data$label, colour = .data$sig)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "gray50") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high),
                            linewidth = 0.7) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "gray60", `TRUE` = "#E15759"),
                                 labels = c(`FALSE` = "p \u2265 0.05", `TRUE` = "p < 0.05"),
                                 drop = FALSE) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(title = model$outcome_label,
                  subtitle = ui_text("fig_forest_sub", lang),
                  x = ui_text("fig_or_log", lang), y = NULL,
                  colour = ui_text("fig_significance", lang),
                  caption = sprintf("n = %d, %s = %d | Nagelkerke R2 = %.3f | Hosmer-Lemeshow p = %s",
                                    f$n, ui_text("fig_events", lang), f$events, f$nagelkerke,
                                    if (is.na(f$hl_p)) "NA" else sprintf("%.3f", f$hl_p))) +
    theme_registat() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "gray90",
                                                              linetype = "dashed"),
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Predictor-by-outcome matrix
#'
#' For several outcomes at once: which predictors stayed in each model
#' ([regression_matrix()]). The shape tells the direction (circle: risk
#' factor, adjusted odds ratio above 1; square: protective factor, below 1)
#' and the colour and size the significance. The AUC of each model is in the
#' row label. Predictors appearing in more models come first.
#'
#' @param x An object from [regression_matrix()].
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(400, seed = 1)$data, load_template("t2d"), "en")
#' rm <- regression_matrix(a, c("ckd", "hypertension"), c("age_years", "hba1c_pct"),
#'                         selection = "none")
#' plot_predictor_matrix(rm)
#' @export
plot_predictor_matrix <- function(x) {
  if (!inherits(x, "registat_models")) abort("'x' must come from regression_matrix().")
  d <- x$matrix
  if (nrow(d) == 0L) abort("No model kept any predictor.")
  lang <- x$lang
  s <- x$summary
  lab_y <- stats::setNames(sprintf("%s (AUC %.3f)", s$outcome_label, s$auc), s$outcome)
  d$row <- factor(lab_y[d$outcome], levels = unname(lab_y))
  ord <- names(sort(table(d$label), decreasing = TRUE))
  d$label <- factor(d$label, levels = rev(ord))
  d$direction <- factor(d$direction, levels = c("risk", "protective"),
                        labels = c(ui_text("fig_risk", lang), ui_text("fig_protective", lang)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$label, y = .data$row)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$significant, shape = .data$direction,
                                     size = .data$significant)) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#E75480", `FALSE` = "black"),
                                 labels = c(`TRUE` = "p < 0.05", `FALSE` = "p \u2265 0.05"),
                                 name = ui_text("fig_significance", lang)) +
    ggplot2::scale_shape_manual(values = stats::setNames(
      c(16, 15), levels(d$direction)), name = ui_text("fig_effect", lang), drop = FALSE) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 6, `FALSE` = 3.5), guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_registat() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.9, hjust = 0.9),
                   panel.grid.major.x = ggplot2::element_line(colour = "gray90",
                                                              linetype = "dashed"),
                   legend.position = "bottom")
}

#' ROC curve of the fitted probabilities of a model
#'
#' @param model An object from [logistic_model()].
#' @return A `ggplot` object with the curve, the AUC and its 95% interval,
#'   and the point that maximises Youden's index.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' plot_model_roc(logistic_model(a, "ckd", c("age_years", "hba1c_pct"), selection = "none"))
#' @export
plot_model_roc <- function(model) {
  if (!inherits(model, "registat_model")) abort("'model' must come from logistic_model().")
  if (is.null(model$roc)) abort("The model has no predictors.")
  lang <- model$lang
  r <- model$roc
  cv <- r$curve[is.finite(r$curve$threshold) | TRUE, , drop = FALSE]
  cv$fpr <- 1 - cv$specificity
  b <- r$best
  txt <- c(sprintf("AUC = %.3f (95%% CI %.3f-%.3f)", r$auc, r$ci_low, r$ci_high),
           sprintf("%s = %.3f", ui_text("fig_cutoff_prob", lang), b$threshold),
           sprintf("%s = %.1f%% | %s = %.1f%%", ui_text("col_sensitivity", lang),
                   100 * b$sensitivity, ui_text("col_specificity", lang), 100 * b$specificity))
  ggplot2::ggplot(cv, ggplot2::aes(x = .data$fpr, y = .data$sensitivity)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "gray60", linetype = "dashed") +
    ggplot2::geom_area(fill = "#E15759", alpha = 0.12) +
    ggplot2::geom_path(colour = "#E15759", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(fpr = 1 - b$specificity, sensitivity = b$sensitivity),
                        colour = "#4E79A7", size = 3.5) +
    ggplot2::annotate("text", x = 1, y = c(0.18, 0.11, 0.04), label = txt, hjust = 1, size = 3.3) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = sprintf("%s - %s", ui_text("fig_roc", lang), model$outcome_label),
                  x = ui_text("fig_1_spec", lang), y = ui_text("col_sensitivity", lang)) +
    theme_registat() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "gray90",
                                                              linetype = "dashed"))
}

#' Comparison of the ROC curves of several biomarkers for one outcome
#'
#' @param tab An object from [roc_biomarkers()].
#' @param outcome The outcome variable to draw.
#' @return A `ggplot` object with one curve per biomarker and the AUC in the
#'   legend.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' tab <- roc_biomarkers(a, "ckd", c("creatinine_mgdl", "hba1c_pct", "egfr"))
#' plot_roc(tab, "ckd")
#' @export
plot_roc <- function(tab, outcome) {
  if (!inherits(tab, "registat_roc_table")) abort("'tab' must come from roc_biomarkers().")
  lang <- attr(tab, "lang") %||% "en"
  sub <- tab[tab$outcome == outcome, , drop = FALSE]
  if (nrow(sub) == 0L) abort("No ROC curves for outcome '%s'.", outcome)
  parts <- lapply(seq_len(nrow(sub)), function(i) {
    r <- roc_of(tab, outcome, sub$biomarker[i])
    data.frame(fpr = 1 - r$curve$specificity, sens = r$curve$sensitivity,
               curve = sprintf("%s (AUC=%.3f)", sub$biomarker_label[i], sub$auc[i]),
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, parts)
  d$curve <- factor(d$curve, levels = unique(d$curve))
  cols <- stats::setNames(rep_len(.roc_palette, nlevels(d$curve)), levels(d$curve))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$fpr, y = .data$sens, colour = .data$curve)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "gray60") +
    ggplot2::geom_path(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = sprintf("%s - %s", ui_text("fig_roc", lang), sub$outcome_label[1L]),
                  subtitle = ui_text("fig_roc_sub", lang),
                  x = ui_text("fig_1_spec", lang), y = ui_text("col_sensitivity", lang),
                  colour = NULL) +
    theme_registat() +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 8),
                   panel.grid.major.x = ggplot2::element_line(colour = "gray90",
                                                              linetype = "dashed"))
}

#' Heat map of the AUC of biomarkers by outcome
#'
#' Each cell shows the AUC and the Youden cut-off; the colour is the verbal
#' class of the AUC (outstanding, excellent, acceptable, unsatisfactory, no
#' discrimination).
#'
#' @param tab An object from [roc_biomarkers()].
#' @return A `ggplot` object.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' tab <- roc_biomarkers(a, c("ckd", "hypertension"), c("creatinine_mgdl", "hba1c_pct"))
#' plot_auc_heatmap(tab)
#' @export
plot_auc_heatmap <- function(tab) {
  if (!inherits(tab, "registat_roc_table")) abort("'tab' must come from roc_biomarkers().")
  if (nrow(tab) == 0L) abort("There are no ROC results to draw.")
  lang <- attr(tab, "lang") %||% "en"
  d <- as.data.frame(tab)
  d$class <- auc_class(d$auc, lang)
  d$text <- sprintf("AUC = %.3f\n(%s:\n%s)", d$auc, ui_text("fig_cutoff", lang),
                    formatC(d$cutoff, format = "fg", digits = 3))
  d$outcome_label <- factor(d$outcome_label, levels = unique(d$outcome_label))
  d$biomarker_label <- factor(d$biomarker_label, levels = rev(unique(d$biomarker_label)))
  cols <- stats::setNames(unname(.auc_colours), levels(d$class))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$outcome_label, y = .data$biomarker_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$class), colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = .data$text), size = 2.4, lineheight = 1.05,
                       colour = "grey30") +
    ggplot2::scale_fill_manual(values = cols, drop = FALSE,
                               name = ui_text("fig_diag_capacity", lang)) +
    ggplot2::labs(x = ui_text("fig_comp_comorb", lang), y = ui_text("fig_biomarker", lang)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, face = "bold",
                                                       colour = "grey20"),
                   axis.text.y = ggplot2::element_text(face = "bold", colour = "grey20"),
                   panel.grid = ggplot2::element_blank(), legend.position = "right")
}

#' Save a figure as PNG, SVG or PDF
#'
#' The format follows the file extension. SVG uses the svglite package when
#' it is installed and the base graphics SVG device otherwise; PDF uses the
#' Cairo device when available so that accents are embedded.
#'
#' @param plot A `ggplot` object.
#' @param file Output path ending in `.png`, `.svg` or `.pdf`.
#' @param width,height Size in millimetres.
#' @param dpi Resolution of PNG files.
#' @return `file`, invisibly.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(100, seed = 1)$data, load_template("t2d"), "en")
#' f <- tempfile(fileext = ".png")
#' save_figure(plot_pyramid(a), f)
#' @export
save_figure <- function(plot, file, width = 180, height = 120, dpi = 300) {
  ext <- tolower(tools::file_ext(file))
  if (!(ext %in% c("png", "svg", "pdf"))) abort("The file must end in .png, .svg or .pdf.")
  if (ext == "png") {
    ggplot2::ggsave(file, plot, width = width, height = height, units = "mm", dpi = dpi,
                    bg = "white")
  } else if (ext == "svg") {
    dev <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else
      grDevices::svg
    ggplot2::ggsave(file, plot, width = width, height = height, units = "mm",
                    device = dev, bg = "white")
  } else {
    dev <- if (isTRUE(unname(capabilities("cairo")))) grDevices::cairo_pdf else
      grDevices::pdf
    ggplot2::ggsave(file, plot, width = width, height = height, units = "mm",
                    device = dev, bg = "white")
  }
  invisible(file)
}
