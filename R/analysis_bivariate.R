# Bivariate analysis: normality, comparison of a yes/no outcome across
# continuous and categorical variables, correlations and prevalences.
# Every function takes an object from analysis_prepare() and returns a plain
# data frame with stable column names; see localize_table() for display.

#' Yes/no variables of an analysis object
#'
#' Variables whose dictionary choices include both `"yes"` and `"no"`, plus
#' the calculated "at least one" flags, can be used as outcomes of the
#' bivariate, regression and ROC analyses.
#'
#' @param a An object from [analysis_prepare()].
#' @return A named character vector (names are labels, values are variable
#'   names).
#' @examples
#' a <- analysis_prepare(synthetic_t2d(60, seed = 1)$data, load_template("t2d"), "en")
#' binary_variables(a)
#' @export
binary_variables <- function(a) {
  check_analysis(a)
  nm <- names(a$binary)
  stats::setNames(nm, unname(a$labels[nm]))
}

#' Outcome as 0/1 (internal)
#'
#' 1 = the "yes" level, 0 = the "no" level, `NA` for anything else
#' (for example "Unknown" or not recorded).
#' @noRd
binary_outcome <- function(a, var) {
  check_analysis(a)
  b <- a$binary[[var]]
  if (is.null(b)) abort("'%s' is not a yes/no variable.", var)
  x <- as.character(a$data[[var]])
  ifelse(x == b[["positive"]], 1L, ifelse(x == b[["negative"]], 0L, NA_integer_))
}

#' A categorical variable with its "unknown" level set to missing (internal)
#' @noRd
clean_factor <- function(a, var) {
  x <- a$data[[var]]
  cd <- a$codes[[var]]
  unk_label <- unname(cd[names(cd) == "unknown"])
  if (length(unk_label)) x[x %in% unk_label] <- NA
  droplevels(x)
}

#' Names of the "calculated at least one" flags and counts (internal)
#' @noRd
aggregate_vars <- function() {
  c("n_complications", "any_complication", "n_comorbidities", "any_comorbidity",
    "n_comp_comorb", "any_comp_comorb")
}

#' @noRd
normal_flag <- function(x, alpha = 0.05) {
  x <- x[!is.na(x)]
  if (length(x) < 3L || length(x) > 5000L || stats::var(x) == 0) return(NA)
  stats::shapiro.test(x)$p.value > alpha
}

#' Normality of the numeric variables (Shapiro-Wilk)
#'
#' The Shapiro-Wilk test needs between 3 and 5000 values and some variation;
#' otherwise the result is `NA`. A variable counts as approximately normal
#' when p is above `alpha`. With few records the test has little power, so
#' look at the histograms ([plot_normality()]) as well.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Numeric variables (default: all).
#' @param alpha Significance level.
#' @return A data frame with `variable`, `label`, `n`, `w`, `p_value` and
#'   `normal` (logical).
#' @examples
#' a <- analysis_prepare(synthetic_t2d(80, seed = 1)$data, load_template("t2d"), "en")
#' normality_table(a, c("age_years", "hba1c_pct"))
#' @export
normality_table <- function(a, vars = NULL, alpha = 0.05) {
  check_analysis(a)
  vars <- vars %||% names(a$types)[a$types == "numeric"]
  for (v in vars) check_var(a, v, "numeric")
  rows <- lapply(vars, function(v) {
    x <- a$data[[v]]
    x <- x[!is.na(x)]
    ok <- length(x) >= 3L && length(x) <= 5000L && stats::var(x) > 0
    st <- if (ok) stats::shapiro.test(x) else NULL
    data.frame(variable = v, label = a$labels[[v]], n = length(x),
               w = if (ok) unname(st$statistic) else NA_real_,
               p_value = if (ok) st$p.value else NA_real_,
               normal = if (ok) st$p.value > alpha else NA,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Compare numeric variables between patients with and without an outcome
#'
#' For each numeric variable: the summary in everyone and in each outcome
#' group, and a p-value. Variables that look normal (Shapiro-Wilk on all
#' non-missing values, see [normality_table()]) are compared with Welch's
#' t-test; the others with the Wilcoxon rank-sum (Mann-Whitney) test. The
#' summary is median (Q1 - Q3) or mean (SD). P-values are not adjusted for
#' multiple comparisons.
#'
#' @param a An object from [analysis_prepare()].
#' @param outcome A yes/no variable, see [binary_variables()].
#' @param vars Numeric variables (default: all except the calculated counts).
#' @param summary `"median"` (default) or `"mean"`.
#' @param alpha Level used to decide normality.
#' @param digits Decimals.
#' @return A data frame with `variable`, `label`, the overall column, one
#'   column per outcome group, `n_missing`, `test` and `p_value`.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(120, seed = 1)$data, load_template("t2d"), "en")
#' compare_continuous(a, "ckd", c("age_years", "creatinine_mgdl"))
#' @export
compare_continuous <- function(a, outcome, vars = NULL, summary = c("median", "mean"),
                               alpha = 0.05, digits = 1L) {
  check_analysis(a)
  summary <- match.arg(summary)
  y <- binary_outcome(a, outcome)
  vars <- vars %||% setdiff(names(a$types)[a$types == "numeric"],
                            c(outcome, "n_complications", "n_comorbidities", "n_comp_comorb"))
  for (v in vars) check_var(a, v, "numeric")
  b <- a$binary[[outcome]]
  lang <- a$lang
  fmt <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return("")
    if (summary == "median") {
      q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
      sprintf("%s (%s - %s)", fmt_num(q[2L], digits), fmt_num(q[1L], digits),
              fmt_num(q[3L], digits))
    } else {
      sprintf("%s (%s)", fmt_num(mean(x), digits),
              if (length(x) > 1L) fmt_num(stats::sd(x), digits) else "")
    }
  }
  n_all <- sum(!is.na(y))
  nm <- c(sprintf("%s (n=%d)", ui_text("tbl_overall", lang), n_all),
          sprintf("%s (n=%d)", b[["negative"]], sum(y == 0L, na.rm = TRUE)),
          sprintf("%s (n=%d)", b[["positive"]], sum(y == 1L, na.rm = TRUE)))
  rows <- lapply(vars, function(v) {
    x <- a$data[[v]]
    keep <- !is.na(y)
    x0 <- x[keep & y == 0L]
    x1 <- x[keep & y == 1L]
    isn <- normal_flag(x[keep], alpha)
    use_t <- isTRUE(isn)
    p <- tryCatch({
      xx <- c(x0, x1)
      gg <- c(rep(0L, length(x0)), rep(1L, length(x1)))
      ok <- !is.na(xx)
      if (sum(gg[ok] == 0L) < 2L || sum(gg[ok] == 1L) < 2L) NA_real_
      else if (use_t) stats::t.test(xx[ok] ~ gg[ok])$p.value
      else suppressWarnings(stats::wilcox.test(xx[ok] ~ gg[ok])$p.value)
    }, error = function(e) NA_real_)
    r <- data.frame(variable = v, label = a$labels[[v]], stringsAsFactors = FALSE)
    r[[nm[1L]]] <- fmt(x[keep])
    r[[nm[2L]]] <- fmt(x0)
    r[[nm[3L]]] <- fmt(x1)
    r$n_missing <- sum(is.na(x[keep]))
    r$test <- ui_text(if (use_t) "test_welch" else "test_wilcoxon", lang)
    r$p_value <- p
    r
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "outcome") <- outcome
  out
}

#' Association of categorical variables with a yes/no outcome
#'
#' For each categorical variable (and each outcome) a 2 x k table is tested
#' with the chi-squared test (with continuity correction for 2 x 2 tables),
#' or with Fisher's exact test when any expected count is below 5 (an exact
#' test for 2 x 2 tables, a simulated one with 2000 replicates and a fixed
#' seed for larger tables). For 2 x 2 tables the crude odds ratio compares
#' the second level of the variable with the first (for yes/no variables,
#' "yes" against "no"); its 95% interval is Wald (chi-squared) or conditional
#' exact (Fisher), and it is `NA` when a cell is empty under the chi-squared
#' path. "Unknown" answers are left out. P-values are not adjusted.
#'
#' @param a An object from [analysis_prepare()].
#' @param outcomes One or more yes/no variables.
#' @param exposures Categorical variables (default: every categorical
#'   variable except the outcome and the calculated "at least one" flags).
#' @return A data frame with `outcome`, `outcome_label`, `exposure`,
#'   `exposure_label`, `compared` (the two levels compared), `n`, `test`,
#'   `or`, `ci_low`, `ci_high` and `p_value`.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(150, seed = 1)$data, load_template("t2d"), "en")
#' associations_categorical(a, "ckd", c("sex_at_birth", "hypertension"))
#' @export
associations_categorical <- function(a, outcomes, exposures = NULL) {
  check_analysis(a)
  cats <- names(a$types)[a$types == "categorical"]
  rows <- list()
  for (out in outcomes) {
    y <- binary_outcome(a, out)
    b <- a$binary[[out]]
    exps <- exposures %||% setdiff(cats, c(out, aggregate_vars()))
    for (ex in exps) {
      if (identical(ex, out)) next
      check_var(a, ex, "categorical")
      x <- model_variable(a, ex)
      ok <- !is.na(x) & !is.na(y)
      xx <- droplevels(x[ok])
      yy <- factor(y[ok], levels = c(0L, 1L), labels = c(b[["negative"]], b[["positive"]]))
      if (nlevels(xx) < 2L || nlevels(yy) < 2L) next
      tab <- table(xx, yy)
      r <- one_association(tab)
      rows[[length(rows) + 1L]] <- data.frame(
        outcome = out, outcome_label = a$labels[[out]], exposure = ex,
        exposure_label = a$labels[[ex]],
        compared = if (nlevels(xx) == 2L) sprintf("%s / %s", levels(xx)[2L], levels(xx)[1L]) else "",
        n = sum(tab), test = ui_text(r$test, a$lang), or = r$or, ci_low = r$ci_low,
        ci_high = r$ci_high, p_value = r$p, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(outcome = character(), outcome_label = character(),
                      exposure = character(), exposure_label = character(),
                      compared = character(), n = integer(), test = character(),
                      or = numeric(), ci_low = numeric(), ci_high = numeric(),
                      p_value = numeric(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Test and odds ratio of one table (internal)
#' @noRd
one_association <- function(tab) {
  es_2x2 <- all(dim(tab) == c(2L, 2L))
  expected <- tryCatch(suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected),
                       error = function(e) matrix(0))
  or <- ci_low <- ci_high <- NA_real_
  if (any(expected < 5)) {
    if (es_2x2) {
      f <- stats::fisher.test(tab)
      p <- f$p.value
      or <- unname(f$estimate)
      ci_low <- f$conf.int[1L]
      ci_high <- f$conf.int[2L]
    } else {
      p <- withr::with_seed(1L, stats::fisher.test(tab, simulate.p.value = TRUE,
                                                   B = 2000L)$p.value)
    }
    test <- "test_fisher"
  } else {
    p <- stats::chisq.test(tab, correct = TRUE)$p.value
    test <- "test_chisq"
    if (es_2x2 && all(tab > 0)) {
      a1 <- tab[2L, 2L]; b1 <- tab[2L, 1L]; c1 <- tab[1L, 2L]; d1 <- tab[1L, 1L]
      or <- (a1 * d1) / (b1 * c1)
      ee <- sqrt(1 / a1 + 1 / b1 + 1 / c1 + 1 / d1)
      ci_low <- exp(log(or) - 1.96 * ee)
      ci_high <- exp(log(or) + 1.96 * ee)
    }
  }
  list(test = test, p = p, or = or, ci_low = ci_low, ci_high = ci_high)
}

#' Pairwise correlations of numeric variables
#'
#' Correlation coefficient and test for every pair of variables, using the
#' records that have both values. `method = "auto"` uses Pearson when both
#' variables look normal (Shapiro-Wilk) and Spearman otherwise. P-values are
#' adjusted over all the pairs (default Holm). Spearman's test uses the
#' asymptotic approximation.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Numeric variables (at least two).
#' @param method `"spearman"` (default), `"pearson"` or `"auto"`.
#' @param adjust Method of [stats::p.adjust()].
#' @return A data frame with `var1`, `var2`, `label1`, `label2`, `method`,
#'   `n`, `r`, `p_value` and `p_adj`, strongest correlations first.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(100, seed = 1)$data, load_template("t2d"), "en")
#' correlations(a, c("age_years", "hba1c_pct", "fasting_glucose_mgdl"))
#' @export
correlations <- function(a, vars, method = c("spearman", "pearson", "auto"),
                         adjust = "holm") {
  check_analysis(a)
  method <- match.arg(method)
  vars <- unique(vars)
  if (length(vars) < 2L) abort("Choose at least two numeric variables.")
  for (v in vars) check_var(a, v, "numeric")
  pairs <- utils::combn(vars, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(p) {
    x <- a$data[[p[1L]]]
    y <- a$data[[p[2L]]]
    ok <- !is.na(x) & !is.na(y)
    m <- if (method == "auto") {
      if (isTRUE(normal_flag(x)) && isTRUE(normal_flag(y))) "pearson" else "spearman"
    } else method
    ct <- if (sum(ok) >= 4L && stats::var(x[ok]) > 0 && stats::var(y[ok]) > 0) {
      suppressWarnings(stats::cor.test(x[ok], y[ok], method = m, exact = FALSE))
    } else NULL
    data.frame(var1 = p[1L], var2 = p[2L], label1 = a$labels[[p[1L]]],
               label2 = a$labels[[p[2L]]], method = m, n = sum(ok),
               r = if (is.null(ct)) NA_real_ else unname(ct$estimate),
               p_value = if (is.null(ct)) NA_real_ else ct$p.value,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$p_adj <- stats::p.adjust(out$p_value, method = adjust)
  out <- out[order(-abs(out$r), na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "adjust") <- adjust
  out
}

#' Prevalence of yes/no variables, overall and by group
#'
#' Number with "yes", number with a yes or no answer, percentage and exact
#' (Clopper-Pearson) 95% confidence interval, for everyone and for each level
#' of a grouping variable. "Unknown" and unanswered are not counted in the
#' denominator.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Yes/no variables (default: the dictionary's yes/no variables
#'   without the calculated counts).
#' @param by Optional categorical grouping variable (for example sex at
#'   birth).
#' @param conf_level Confidence level.
#' @return A long data frame with `variable`, `label`, `group`, `n`, `N`,
#'   `pct`, `ci_low` and `ci_high` (percentages).
#' @examples
#' a <- analysis_prepare(synthetic_t2d(100, seed = 1)$data, load_template("t2d"), "en")
#' prevalence_table(a, c("hypertension", "ckd"), by = "sex_at_birth")
#' @export
prevalence_table <- function(a, vars = NULL, by = NULL, conf_level = 0.95) {
  check_analysis(a)
  vars <- vars %||% setdiff(names(a$binary), aggregate_vars())
  if (!is.null(by)) check_var(a, by, "categorical")
  g <- if (is.null(by)) NULL else clean_factor(a, by)
  groups <- c(ui_text("tbl_overall", a$lang), if (!is.null(g)) levels(g))
  rows <- list()
  for (v in vars) {
    y <- binary_outcome(a, v)
    for (k in seq_along(groups)) {
      idx <- if (k == 1L) rep(TRUE, length(y)) else !is.na(g) & g == groups[k]
      yy <- y[idx & !is.na(y)]
      ci <- proportion_ci(sum(yy), length(yy), conf_level)
      rows[[length(rows) + 1L]] <- data.frame(
        variable = v, label = a$labels[[v]], group = groups[k], n = sum(yy),
        N = length(yy), pct = if (length(yy)) 100 * sum(yy) / length(yy) else NA_real_,
        ci_low = 100 * ci[1L], ci_high = 100 * ci[2L], stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$group <- factor(out$group, levels = groups)
  out
}

#' Extreme values by the IQR rule
#'
#' Flags, for each numeric variable, the values beyond `k` interquartile
#' ranges below the first or above the third quartile (default `k = 3`, the
#' "far out" fences of Tukey). Flagged values are not removed or changed:
#' they are candidates to check against the source record.
#'
#' @param a An object from [analysis_prepare()].
#' @param vars Numeric variables (default: all numeric analysis variables
#'   except the calculated counts).
#' @param k Number of interquartile ranges.
#' @return A data frame with `variable`, `label`, `n`, `q1`, `q3`, `lower`,
#'   `upper`, `n_outliers`, `rows` (row positions in the analysis data) and
#'   `values`, most flagged first.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(100, seed = 1)$data, load_template("t2d"), "en")
#' outlier_summary(a)
#' @export
outlier_summary <- function(a, vars = NULL, k = 3) {
  check_analysis(a)
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k <= 0) abort("'k' must be a positive number.")
  nums <- names(a$types)[a$types == "numeric"]
  vars <- vars %||% setdiff(nums, c("n_complications", "n_comorbidities", "n_comp_comorb"))
  for (v in vars) check_var(a, v, "numeric")
  rows <- lapply(vars, function(v) {
    x <- a$data[[v]]
    y <- x[!is.na(x)]
    if (length(y) < 4L) {
      return(data.frame(variable = v, label = a$labels[[v]], n = length(y), q1 = NA_real_,
                        q3 = NA_real_, lower = NA_real_, upper = NA_real_, n_outliers = 0L,
                        rows = "", values = "", stringsAsFactors = FALSE))
    }
    q <- stats::quantile(y, c(0.25, 0.75), names = FALSE, type = 7)
    iqr <- q[2L] - q[1L]
    lo <- q[1L] - k * iqr
    hi <- q[2L] + k * iqr
    idx <- which(!is.na(x) & (x < lo | x > hi))
    data.frame(variable = v, label = a$labels[[v]], n = length(y), q1 = q[1L], q3 = q[2L],
               lower = lo, upper = hi, n_outliers = length(idx),
               rows = paste(idx, collapse = ", "),
               values = paste(signif(x[idx], 4), collapse = ", "),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$n_outliers, out$variable), , drop = FALSE]
  rownames(out) <- NULL
  out
}
