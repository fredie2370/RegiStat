# ROC analysis of individual biomarkers for yes/no outcomes.

#' ROC analysis of biomarkers for one or more outcomes
#'
#' For every combination of numeric biomarker and yes/no outcome: the area
#' under the ROC curve with its 95% DeLong interval, a verbal interpretation
#' (outstanding >= 0.90, excellent >= 0.80, acceptable >= 0.70,
#' unsatisfactory >= 0.60, otherwise no discrimination), and the cut-off that
#' maximises Youden's index (sensitivity + specificity - 1) with its
#' sensitivity, specificity and predictive values.
#'
#' The direction is chosen automatically from the data (higher or lower
#' values indicating the outcome; see `direction` in the result). The cut-off
#' is the midpoint between two consecutive observed values; the event is
#' predicted when the biomarker is above it (`direction == "<"`) or below it
#' (`direction == ">"`). Cut-offs, sensitivity and specificity are computed
#' on the same data used to find them, so they are optimistic; predictive
#' values depend on the prevalence of the outcome in this sample.
#' Biomarkers that are part of the definition of the outcome (for example
#' eGFR and chronic kidney disease) will naturally discriminate well.
#' Combinations with fewer than `min_events` events or non-events are
#' skipped and listed in the `skipped` attribute.
#'
#' @param a An object from [analysis_prepare()].
#' @param outcomes Yes/no variables.
#' @param biomarkers Numeric variables.
#' @param min_events Minimum number of events and of non-events.
#' @return A data frame of class `registat_roc_table` with `outcome`,
#'   `outcome_label`, `biomarker`, `biomarker_label`, `n`, `events`,
#'   `direction`, `auc`, `ci_low`, `ci_high`, `interpretation`, `cutoff`,
#'   `sensitivity`, `specificity`, `ppv`, `npv` (percentages for the last
#'   four). The attribute `rocs` holds the curves, named
#'   `"outcome|biomarker"`.
#' @examples
#' a <- analysis_prepare(synthetic_t2d(300, seed = 1)$data, load_template("t2d"), "en")
#' roc_biomarkers(a, "ckd", c("creatinine_mgdl", "hba1c_pct"))
#' @export
roc_biomarkers <- function(a, outcomes, biomarkers, min_events = 10L) {
  check_analysis(a)
  for (b in biomarkers) check_var(a, b, "numeric")
  rows <- list()
  rocs <- list()
  skipped <- data.frame(outcome = character(), biomarker = character(),
                        reason = character(), stringsAsFactors = FALSE)
  for (o in outcomes) {
    y <- binary_outcome(a, o)
    for (b in setdiff(biomarkers, o)) {
      x <- a$data[[b]]
      ok <- !is.na(y) & !is.na(x)
      ne <- sum(y[ok] == 1L)
      nn <- sum(y[ok] == 0L)
      if (ne < min_events || nn < min_events) {
        skipped <- rbind(skipped, data.frame(
          outcome = o, biomarker = b, stringsAsFactors = FALSE,
          reason = ui_text("roc_skip", a$lang, ne, nn)))
        next
      }
      r <- tryCatch(roc_curve(y, x, "auto"), error = function(e) NULL)
      if (is.null(r)) next
      rocs[[paste(o, b, sep = "|")]] <- r
      rows[[length(rows) + 1L]] <- data.frame(
        outcome = o, outcome_label = a$labels[[o]], biomarker = b,
        biomarker_label = a$labels[[b]], n = sum(ok), events = ne,
        direction = r$direction, auc = r$auc, ci_low = r$ci_low, ci_high = r$ci_high,
        interpretation = as.character(auc_class(r$auc, a$lang)),
        cutoff = r$best$threshold, sensitivity = 100 * r$best$sensitivity,
        specificity = 100 * r$best$specificity, ppv = 100 * r$best$ppv,
        npv = 100 * r$best$npv, stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  rownames(out) <- NULL
  structure(out, rocs = rocs, skipped = skipped, lang = a$lang,
            class = c("registat_roc_table", "data.frame"))
}

#' ROC curve of one biomarker (internal)
#' @noRd
roc_of <- function(tab, outcome, biomarker) {
  attr(tab, "rocs")[[paste(outcome, biomarker, sep = "|")]]
}
