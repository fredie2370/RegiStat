# Statistical building blocks. They use base R only, so that RegiStat does not
# depend on extra packages for its core analyses. They are checked against
# the pROC and car packages (when installed) in the test suite.

#' ROC curve, AUC with DeLong interval and Youden cut-off (internal)
#'
#' @param response 0/1 vector (1 = event). Missing pairs are dropped.
#' @param predictor Numeric vector.
#' @param direction `"auto"`, `"<"` or `">"`. `"<"`: higher values indicate the
#'   event; `">"`: lower values do. `"auto"` picks `"<"` when the median of the
#'   non-events is not above the median of the events, as pROC does. With
#'   `"auto"` the AUC can therefore be below 0.5 when the two medians are
#'   nearly equal.
#' @param conf_level Confidence level of the interval.
#' @return A list of class `registat_roc`: `direction`, `auc`, `ci_low`,
#'   `ci_high`, `n_cases`, `n_controls`, `curve` (data frame with
#'   `threshold`, `sensitivity`, `specificity`), `best` (the row of `curve`
#'   with the largest Youden index J = sensitivity + specificity - 1, first one
#'   in case of ties, with `ppv` and `npv`).
#' @noRd
roc_curve <- function(response, predictor, direction = c("auto", "<", ">"),
                      conf_level = 0.95) {
  direction <- match.arg(direction)
  ok <- !is.na(response) & !is.na(predictor)
  y <- as.integer(response[ok])
  x <- as.numeric(predictor[ok])
  if (!all(y %in% 0:1)) abort("The response must contain only 0 and 1.")
  m <- sum(y == 1L)
  n <- sum(y == 0L)
  if (m == 0L || n == 0L) abort("An ROC curve needs both events and non-events.")
  if (direction == "auto") {
    direction <- if (stats::median(x[y == 0L]) <= stats::median(x[y == 1L])) "<" else ">"
  }
  sgn <- if (direction == "<") 1 else -1
  sc <- sgn * x

  # DeLong: structural components from mid-ranks
  r_all <- rank(sc)
  r_case <- rank(sc[y == 1L])
  r_ctrl <- rank(sc[y == 0L])
  v10 <- (r_all[y == 1L] - r_case) / n
  v01 <- 1 - (r_all[y == 0L] - r_ctrl) / m
  auc <- mean(v10)
  se <- sqrt(stats::var(v10) / m + stats::var(v01) / n)
  if (!is.finite(se)) se <- 0
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci <- pmin(pmax(auc + c(-1, 1) * z * se, 0), 1)

  # Curve: thresholds halfway between consecutive distinct scores
  u <- sort(unique(sc))
  k <- length(u)
  idx <- match(sc, u)
  cc <- tabulate(idx[y == 1L], k)
  cn <- tabulate(idx[y == 0L], k)
  sens <- c(1, 1 - cumsum(cc) / m)
  spec <- c(0, cumsum(cn) / n)
  thr_s <- c(-Inf, (u[-1L] + u[-k]) / 2, Inf)
  thr <- if (k == 1L) c(-Inf, Inf) else sgn * thr_s
  if (k == 1L) { sens <- c(1, 0); spec <- c(0, 1) }
  curve <- data.frame(threshold = thr, sensitivity = sens, specificity = spec)
  if (direction == ">") curve <- curve[rev(seq_len(nrow(curve))), , drop = FALSE]
  rownames(curve) <- NULL

  finite <- is.finite(curve$threshold)
  j <- curve$sensitivity + curve$specificity - 1
  j[!finite] <- -Inf
  b <- which.max(j)
  best <- curve[b, , drop = FALSE]
  tp <- best$sensitivity * m
  fp <- (1 - best$specificity) * n
  tn <- best$specificity * n
  fn <- (1 - best$sensitivity) * m
  best$ppv <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  best$npv <- if (tn + fn > 0) tn / (tn + fn) else NA_real_
  best$youden <- best$sensitivity + best$specificity - 1
  rownames(best) <- NULL

  structure(list(direction = direction, auc = auc, ci_low = ci[1L],
                 ci_high = ci[2L], n_cases = m, n_controls = n,
                 curve = curve, best = best),
            class = "registat_roc")
}

#' Verbal interpretation of an AUC (internal)
#'
#' Outstanding >= 0.90, excellent >= 0.80, acceptable >= 0.70, unsatisfactory
#' 0.60 or more, otherwise no discrimination (Hosmer and Lemeshow's scale, with
#' the last two classes named as in the thesis that motivated the module).
#' @return A factor with the five levels, labelled in `lang`.
#' @noRd
auc_class <- function(auc, lang = "en") {
  keys <- c("outstanding", "excellent", "acceptable", "unsatisfactory", "none")
  k <- ifelse(is.na(auc), NA_character_,
              ifelse(auc >= 0.9, "outstanding",
                     ifelse(auc >= 0.8, "excellent",
                            ifelse(auc >= 0.7, "acceptable",
                                   ifelse(auc >= 0.6, "unsatisfactory", "none")))))
  factor(k, levels = keys,
         labels = vapply(keys, function(x) ui_text(paste0("auc_", x), lang), character(1)))
}

#' Hosmer-Lemeshow goodness-of-fit test (internal)
#'
#' Groups are the `g` quantile groups of the fitted probabilities (fewer when
#' ties make some quantiles coincide); the statistic is compared with a
#' chi-squared distribution on (groups - 2) degrees of freedom.
#' @return A list with `statistic`, `df` and `p_value` (`NA` when fewer than
#'   three groups can be formed).
#' @noRd
hosmer_lemeshow <- function(y, p, g = 10L) {
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]
  p <- p[ok]
  q <- unique(stats::quantile(p, probs = seq(0, 1, length.out = g + 1L), names = FALSE))
  if (length(q) < 2L) {
    return(list(statistic = NA_real_, df = NA_integer_, p_value = NA_real_))
  }
  grp <- cut(p, breaks = q, include.lowest = TRUE)
  obs1 <- tapply(y, grp, sum)
  exp1 <- tapply(p, grp, sum)
  cnt <- tapply(y, grp, length)
  obs0 <- cnt - obs1
  exp0 <- cnt - exp1
  keep <- !is.na(obs1)
  stat <- sum((obs1[keep] - exp1[keep])^2 / exp1[keep] +
                (obs0[keep] - exp0[keep])^2 / exp0[keep])
  df <- sum(keep) - 2L
  list(statistic = unname(stat), df = df,
       p_value = if (df >= 1L) stats::pchisq(stat, df, lower.tail = FALSE) else NA_real_)
}

#' Nagelkerke pseudo R-squared of a fitted logistic model (internal)
#' @noRd
nagelkerke_r2 <- function(model) {
  n <- stats::nobs(model)
  ll0 <- -model$null.deviance / 2
  ll1 <- -model$deviance / 2
  cox <- 1 - exp((2 / n) * (ll0 - ll1))
  cox / (1 - exp((2 / n) * ll0))
}

#' Variance inflation factors of a fitted model (internal)
#'
#' One value per model term, from the correlation matrix of the coefficients
#' (generalised VIF of Fox and Monette). For terms with more than one
#' coefficient the value is GVIF^(1/df), which is comparable with an ordinary
#' VIF. Returns `NULL` for models with fewer than two terms.
#' @noRd
vif_values <- function(model) {
  v <- stats::vcov(model)
  assign <- attr(stats::model.matrix(model), "assign")
  keep <- names(stats::coef(model)) != "(Intercept)"
  v <- v[keep, keep, drop = FALSE]
  assign <- assign[keep]
  tl <- attr(stats::terms(model), "term.labels")
  if (length(tl) < 2L) return(NULL)
  r <- stats::cov2cor(v)
  dr <- det(r)
  out <- vapply(seq_along(tl), function(i) {
    ix <- which(assign == i)
    if (length(ix) == 0L) return(NA_real_)
    gvif <- det(r[ix, ix, drop = FALSE]) * det(r[-ix, -ix, drop = FALSE]) / dr
    gvif^(1 / length(ix))
  }, numeric(1))
  stats::setNames(out, tl)
}

#' Exact (Clopper-Pearson) confidence interval of a proportion (internal)
#' @noRd
proportion_ci <- function(x, n, conf_level = 0.95) {
  if (is.na(n) || n <= 0L) return(c(NA_real_, NA_real_))
  as.numeric(stats::binom.test(x, n, conf.level = conf_level)$conf.int)
}
