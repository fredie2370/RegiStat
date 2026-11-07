# Binary logistic regression for a yes/no outcome.

#' Model variables prepared for glm (internal)
#'
#' Numeric variables stay numeric; yes/no variables become factors with "no"
#' as reference; other categorical variables keep their level order (first
#' level = reference) with "unknown" set to missing.
#' @noRd
model_variable <- function(a, v) {
  if (a$types[[v]] == "numeric") return(a$data[[v]])
  if (!is.null(a$binary[[v]])) {
    b <- a$binary[[v]]
    x <- as.character(a$data[[v]])
    x[!(x %in% b)] <- NA
    return(factor(x, levels = c(b[["negative"]], b[["positive"]])))
  }
  clean_factor(a, v)
}

#' Logistic regression of a yes/no outcome
#'
#' Fits a binary logistic model on the records that have the outcome and all
#' chosen predictors (complete cases), optionally followed by backward
#' elimination by AIC ([stats::step()]). Predictors can be numeric or
#' categorical; yes/no predictors use "no" as reference and other categorical
#' predictors their first level. "Unknown" answers count as missing.
#'
#' The result also contains the crude (one predictor at a time) odds ratios,
#' goodness-of-fit and discrimination measures (Nagelkerke R-squared,
#' Hosmer-Lemeshow test, AUC of the fitted probabilities, variance inflation
#' factors) and notes about problems found. Variable selection by AIC is
#' data-driven: it favours the sample at hand and p-values after selection are
#' optimistic, so treat it as exploratory or validate the model elsewhere.
#' As a rule of thumb at least 10 events per estimated coefficient are
#' needed; `epv` reports the actual number.
#'
#' @param a An object from [analysis_prepare()].
#' @param outcome A yes/no variable, see [binary_variables()].
#' @param predictors Variables to enter in the model.
#' @param selection `"backward"` (default) or `"none"` (keep all predictors).
#' @param min_events Minimum number of events (default 10); fewer is an error.
#' @param conf_level Level of the confidence intervals.
#' @return An object of class `registat_model`: a list with `model` (the
#'   `glm`), `data`, `outcome`, `outcome_label`, `predictors` (entered),
#'   `selected` (kept), `n`, `events`, `epv`, `terms` (adjusted odds ratios),
#'   `crude` (crude odds ratios), `fit` (one-row data frame of fit
#'   measures), `vif`, `roc` and `notes`.
#' @seealso [model_table()], [plot_forest()], [regression_matrix()]
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' m <- logistic_model(a, "ckd", c("age_years", "sex_at_birth", "hba1c_pct"))
#' m$terms
#' @export
logistic_model <- function(a, outcome, predictors, selection = c("backward", "none"),
                           min_events = 10L, conf_level = 0.95) {
  check_analysis(a)
  selection <- match.arg(selection)
  y <- binary_outcome(a, outcome)
  predictors <- setdiff(unique(predictors), c(outcome))
  if (length(predictors) == 0L) abort("Choose at least one predictor.")
  for (v in predictors) check_var(a, v)
  notes <- character()

  df <- data.frame(.y = y)
  for (v in predictors) df[[v]] <- model_variable(a, v)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  for (v in predictors) if (is.factor(df[[v]])) df[[v]] <- droplevels(df[[v]])
  const <- vapply(predictors, function(v) {
    x <- df[[v]]
    if (is.factor(x)) nlevels(x) < 2L else length(unique(x)) < 2L
  }, logical(1))
  if (any(const)) {
    notes <- c(notes, ui_text("note_constant", a$lang,
                              paste(a$labels[predictors[const]], collapse = ", ")))
    predictors <- predictors[!const]
  }
  if (length(predictors) == 0L) abort("No predictor varies in the complete cases.")
  n_ev <- sum(df$.y == 1L)
  if (nrow(df) == 0L || n_ev < min_events || sum(df$.y == 0L) < min_events) {
    abort("Too few records: %d events and %d non-events among %d complete cases (at least %d of each are needed).",
          n_ev, sum(df$.y == 0L), nrow(df), min_events)
  }
  for (v in predictors) {
    if (is.factor(df[[v]])) {
      small <- names(which(table(df[[v]]) < 5L))
      if (length(small)) {
        notes <- c(notes, ui_text("note_sparse", a$lang, a$labels[[v]],
                                  paste(small, collapse = ", ")))
      }
    }
  }

  warns <- character()
  fit_glm <- function(formula) {
    withCallingHandlers(
      stats::glm(formula, data = df, family = stats::binomial()),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  }
  full <- fit_glm(stats::reformulate(predictors, response = ".y"))
  final <- if (selection == "backward") {
    withCallingHandlers(stats::step(full, direction = "backward", trace = 0),
                        warning = function(w) invokeRestart("muffleWarning"))
  } else full
  selected <- attr(stats::terms(final), "term.labels")
  if (length(selected) == 0L) {
    notes <- c(notes, ui_text("note_empty_model", a$lang))
  }
  if (any(grepl("fitted probabilities numerically 0 or 1", warns))) {
    notes <- c(notes, ui_text("note_separation", a$lang))
  }
  if (!isTRUE(final$converged)) notes <- c(notes, ui_text("note_noconv", a$lang))

  # Degrees of freedom used by the final model (coefficients without intercept)
  k_par <- length(stats::coef(final)) - 1L
  epv <- if (k_par > 0L) n_ev / k_par else NA_real_
  if (!is.na(epv) && epv < 10) {
    notes <- c(notes, ui_text("note_epv", a$lang, epv, k_par))
  }

  terms_tbl <- if (length(selected)) {
    coef_table(final, a, df, conf_level, notes_env = environment())
  } else coef_table_empty()
  crude <- do.call(rbind, lapply(predictors, function(v) {
    d1 <- data.frame(.y = y, x = model_variable(a, v))
    d1 <- d1[stats::complete.cases(d1), , drop = FALSE]
    if (is.factor(d1$x)) d1$x <- droplevels(d1$x)
    ok <- sum(d1$.y == 1L) >= 2L && sum(d1$.y == 0L) >= 2L &&
      (if (is.factor(d1$x)) nlevels(d1$x) >= 2L else length(unique(d1$x)) >= 2L)
    if (!ok) return(NULL)
    f1 <- suppressWarnings(stats::glm(.y ~ x, data = d1, family = stats::binomial()))
    tb <- coef_table(f1, a, d1, conf_level, var_map = stats::setNames(v, "x"))
    tb
  }))
  if (is.null(crude)) crude <- coef_table_empty()

  p_hat <- as.numeric(stats::fitted(final))
  roc <- if (length(selected)) roc_curve(df$.y, p_hat, direction = "<") else NULL
  hl <- if (length(selected)) hosmer_lemeshow(df$.y, p_hat, 10L) else
    list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_)
  null_fit <- stats::glm(.y ~ 1, data = df, family = stats::binomial())
  fit <- data.frame(
    n = nrow(df), events = n_ev, non_events = nrow(df) - n_ev,
    aic_null = stats::AIC(null_fit), aic = stats::AIC(final),
    loglik = as.numeric(stats::logLik(final)),
    nagelkerke = if (length(selected)) nagelkerke_r2(final) else NA_real_,
    hl_stat = hl$statistic, hl_df = hl$df, hl_p = hl$p_value,
    auc = if (is.null(roc)) NA_real_ else roc$auc,
    auc_low = if (is.null(roc)) NA_real_ else roc$ci_low,
    auc_high = if (is.null(roc)) NA_real_ else roc$ci_high,
    stringsAsFactors = FALSE)
  vif <- if (length(selected) >= 2L) tryCatch(vif_values(final), error = function(e) NULL) else NULL
  if (!is.null(vif) && any(vif > 5, na.rm = TRUE)) {
    notes <- c(notes, ui_text("note_vif", a$lang))
  }

  names(df)[names(df) == ".y"] <- "outcome"
  structure(list(model = final, data = df, outcome = outcome,
                 outcome_label = a$labels[[outcome]], predictors = predictors,
                 selected = selected, selection = selection, n = nrow(df),
                 events = n_ev, epv = epv, terms = terms_tbl, crude = crude,
                 fit = fit, vif = vif, roc = roc, p_hat = p_hat,
                 notes = unique(notes), lang = a$lang,
                 conf_level = conf_level),
            class = "registat_model")
}

#' @export
print.registat_model <- function(x, ...) {
  cat(sprintf("<RegiStat logistic model: %s, n=%d, events=%d, %d term(s) kept of %d>\n",
              x$outcome_label, x$n, x$events, length(x$selected), length(x$predictors)))
  invisible(x)
}

#' @noRd
coef_table_empty <- function() {
  data.frame(term = character(), variable = character(), label = character(),
             or = numeric(), ci_low = numeric(), ci_high = numeric(),
             p_value = numeric(), stringsAsFactors = FALSE)
}

#' Odds-ratio table of a fitted model (internal)
#'
#' Profile-likelihood intervals when available (R >= 4.4, or MASS on older
#' R), Wald intervals otherwise.
#' @noRd
coef_table <- function(fit, a, df, conf_level, var_map = NULL, notes_env = NULL) {
  cf <- summary(fit)$coefficients
  cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]
  if (nrow(cf) == 0L) return(coef_table_empty())
  level <- conf_level
  ci <- tryCatch({
    fn <- if (getRversion() >= "4.4.0") stats::confint else {
      if (requireNamespace("MASS", quietly = TRUE)) utils::getFromNamespace("confint.glm", "MASS") else NULL
    }
    if (is.null(fn)) stop("no profile method")
    suppressMessages(suppressWarnings(fn(fit, level = level)))
  }, error = function(e) NULL)
  if (is.null(ci)) {
    ci <- suppressMessages(stats::confint.default(fit, level = level))
    if (!is.null(notes_env)) {
      notes_env$notes <- c(notes_env$notes, ui_text("note_wald", a$lang))
    }
  }
  # Coefficients, intervals and labels are matched by position, never by
  # name: coefficient names are built from factor levels, and comparing
  # accented names fails in sessions that are not UTF-8.
  ci <- ci[rownames(ci) != "(Intercept)", , drop = FALSE]
  if (nrow(ci) != nrow(cf)) ci <- ci[seq_len(nrow(cf)), , drop = FALSE]
  vars <- if (is.null(var_map)) {
    attr(stats::terms(fit), "term.labels")
  } else unname(var_map)
  asg <- attr(stats::model.matrix(fit), "assign")[!is.na(stats::coef(fit))]
  asg <- asg[asg > 0L]
  variable <- label <- character(nrow(cf))
  seen <- integer(length(vars))
  for (k in seq_len(nrow(cf))) {
    i <- asg[k]
    v <- vars[i]
    col <- if (is.null(var_map)) v else names(var_map)[i]
    x <- df[[col]]
    variable[k] <- v
    if (is.factor(x)) {
      seen[i] <- seen[i] + 1L
      lv <- levels(x)[-1L][seen[i]]
      label[k] <- sprintf("%s: %s vs %s", a$labels[[v]], lv, levels(x)[1L])
    } else {
      label[k] <- a$labels[[v]]
    }
  }
  data.frame(term = rownames(cf), variable = variable, label = label,
             or = exp(cf[, 1L]), ci_low = exp(ci[, 1L]), ci_high = exp(ci[, 2L]),
             p_value = cf[, 4L], stringsAsFactors = FALSE, row.names = NULL)
}

#' Regression table: crude and adjusted odds ratios
#'
#' One row per model coefficient with the crude odds ratio (the predictor
#' alone) and the adjusted one (in the final model), each with its 95%
#' interval and p-value. Predictors dropped by the selection have only the
#' crude columns. Odds ratios for numeric predictors are per one unit.
#'
#' @param model An object from [logistic_model()].
#' @return A data frame with `variable`, `label`, `crude_or`, `crude_ci`,
#'   `crude_p`, `adj_or`, `adj_ci`, `adj_p`.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' m <- logistic_model(a, "ckd", c("age_years", "hba1c_pct"))
#' model_table(m)
#' @export
model_table <- function(model) {
  if (!inherits(model, "registat_model")) abort("'model' must come from logistic_model().")
  cr <- model$crude
  ad <- model$terms
  labs <- unique(c(cr$label, ad$label))
  var_of <- stats::setNames(c(cr$variable, ad$variable), c(cr$label, ad$label))
  f <- function(x, y) sprintf("%s - %s", fmt_num(x, 2L), fmt_num(y, 2L))
  rows <- lapply(labs, function(l) {
    c1 <- cr[cr$label == l, , drop = FALSE]
    a1 <- ad[ad$label == l, , drop = FALSE]
    data.frame(
      variable = unname(var_of[l]), label = l,
      crude_or = if (nrow(c1)) c1$or[1L] else NA_real_,
      crude_ci = if (nrow(c1)) f(c1$ci_low[1L], c1$ci_high[1L]) else "",
      crude_p = if (nrow(c1)) c1$p_value[1L] else NA_real_,
      adj_or = if (nrow(a1)) a1$or[1L] else NA_real_,
      adj_ci = if (nrow(a1)) f(a1$ci_low[1L], a1$ci_high[1L]) else "",
      adj_p = if (nrow(a1)) a1$p_value[1L] else NA_real_,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Logistic models for several outcomes with the same predictors
#'
#' Fits [logistic_model()] for each outcome. Outcomes with too few events are
#' skipped and listed with the reason. The result feeds the summary table
#' of models and the predictor-by-outcome matrix ([plot_predictor_matrix()]).
#'
#' @param a An object from [analysis_prepare()].
#' @param outcomes Yes/no variables.
#' @param predictors Variables entered in every model.
#' @param selection,min_events,conf_level See [logistic_model()].
#' @return A list of class `registat_models` with `models` (named by
#'   outcome), `summary` (one row per model), `matrix` (one row per kept
#'   coefficient: outcome, term label, odds ratio, p-value, `direction` risk
#'   or protective, `significant`) and `skipped`.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(400, seed = 1)$data, load_template("t2d"), "en")
#' rm <- regression_matrix(a, c("ckd", "hypertension"), c("age_years", "hba1c_pct"))
#' rm$summary
#' @export
regression_matrix <- function(a, outcomes, predictors, selection = c("backward", "none"),
                              min_events = 10L, conf_level = 0.95) {
  check_analysis(a)
  selection <- match.arg(selection)
  models <- list()
  skipped <- data.frame(outcome = character(), reason = character(),
                        stringsAsFactors = FALSE)
  for (o in outcomes) {
    m <- tryCatch(logistic_model(a, o, predictors, selection, min_events, conf_level),
                  error = function(e) e)
    if (inherits(m, "error")) {
      skipped <- rbind(skipped, data.frame(outcome = o, reason = conditionMessage(m),
                                           stringsAsFactors = FALSE))
    } else models[[o]] <- m
  }
  summ <- do.call(rbind, lapply(names(models), function(o) {
    m <- models[[o]]
    cbind(data.frame(outcome = o, outcome_label = m$outcome_label,
                     selected = length(m$selected), stringsAsFactors = FALSE), m$fit)
  }))
  mat <- do.call(rbind, lapply(names(models), function(o) {
    m <- models[[o]]
    if (nrow(m$terms) == 0L) return(NULL)
    data.frame(outcome = o, outcome_label = m$outcome_label, variable = m$terms$variable,
               label = m$terms$label, or = m$terms$or, p_value = m$terms$p_value,
               direction = ifelse(m$terms$or > 1, "risk", "protective"),
               significant = m$terms$p_value < 0.05, stringsAsFactors = FALSE)
  }))
  if (is.null(summ)) summ <- data.frame()
  if (is.null(mat)) mat <- data.frame()
  rownames(summ) <- NULL
  rownames(mat) <- NULL
  structure(list(models = models, summary = summ, matrix = mat, skipped = skipped,
                 lang = a$lang), class = "registat_models")
}
