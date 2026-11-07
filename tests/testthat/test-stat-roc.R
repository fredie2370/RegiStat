dict <- load_template("t2d")
prep <- function(n = 228, seed = 3, lang = "en") analysis_prepare(demo_t2d(n, seed), dict, lang)
bio <- c("uric_acid_mgdl", "tyg", "hemoglobin_gdl", "egfr", "creatinine_mgdl")

test_that("roc_biomarkers matches pROC for every combination", {
  skip_if_not_installed("pROC")
  a <- prep()
  tab <- roc_biomarkers(a, c("ckd", "dialysis"), bio)
  expect_s3_class(tab, "registat_roc_table")
  expect_equal(nrow(tab), 10L)
  expect_equal(length(attr(tab, "rocs")), 10L)
  for (i in seq_len(nrow(tab))) {
    y <- binary_outcome(a, tab$outcome[i]); x <- a$data[[tab$biomarker[i]]]
    ok <- !is.na(y) & !is.na(x)
    r <- pROC::roc(y[ok], x[ok], quiet = TRUE, direction = "auto", ci = TRUE)
    co <- pROC::coords(r, "best", best.method = "youden",
                       ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"))
    co <- lapply(as.list(co), function(v) unlist(v)[1])
    expect_equal(tab$auc[i], as.numeric(pROC::auc(r)), tolerance = 1e-10)
    expect_equal(c(tab$ci_low[i], tab$ci_high[i]), as.numeric(pROC::ci(r))[c(1, 3)],
                 tolerance = 1e-8)
    expect_equal(tab$cutoff[i], co$threshold, tolerance = 1e-8)
    expect_equal(tab$sensitivity[i], 100 * co$sensitivity, tolerance = 1e-8)
    expect_equal(tab$specificity[i], 100 * co$specificity, tolerance = 1e-8)
    expect_equal(tab$ppv[i], 100 * co$ppv, tolerance = 1e-8)
    expect_equal(tab$npv[i], 100 * co$npv, tolerance = 1e-8)
    expect_equal(tab$n[i], sum(ok))
    expect_equal(tab$events[i], sum(y[ok]))
  }
})

test_that("direction follows the data (hemoglobin and eGFR are lower in cases)", {
  a <- prep()
  tab <- roc_biomarkers(a, "ckd", bio)
  dir <- stats::setNames(tab$direction, tab$biomarker)
  expect_equal(dir[["creatinine_mgdl"]], "<")
  expect_equal(dir[["hemoglobin_gdl"]], ">")
  expect_equal(dir[["egfr"]], ">")
  expect_true(all(tab$auc >= 0.5 - 1e-9 | tab$biomarker %in% c("tyg")))
})

test_that("a biomarker equal to the outcome is skipped silently", {
  a <- prep()
  tab <- roc_biomarkers(a, "ckd", c("ckd", "egfr")[2])
  expect_equal(nrow(tab), 1L)
  expect_equal(nrow(attr(tab, "skipped")), 0L)
  a2 <- prep()
  a2$types[["n_complications"]] <- "numeric"
  tab2 <- roc_biomarkers(a2, "any_complication", c("n_complications", "egfr"))
  expect_true(all(tab2$biomarker %in% c("n_complications", "egfr")))
})

test_that("combinations with fewer than 10 events are listed as skipped", {
  d <- demo_t2d(228, 3)
  d$amputation <- c("yes", "yes", "yes", rep("no", 225))
  a <- analysis_prepare(d, dict, "en")
  tab <- roc_biomarkers(a, c("ckd", "amputation"), c("egfr", "hemoglobin_gdl"))
  sk <- attr(tab, "skipped")
  expect_equal(nrow(tab), 2L)
  expect_equal(sk$outcome, c("amputation", "amputation"))
  expect_equal(sk$biomarker, c("egfr", "hemoglobin_gdl"))
  expect_match(sk$reason, "Skipped: 3 events")
  expect_error(roc_biomarkers(a, "ckd", "sex_at_birth"), "must be numeric")
  es <- analysis_prepare(d, dict, "es")
  tes <- roc_biomarkers(es, "amputation", "egfr")
  expect_match(attr(tes, "skipped")$reason, "Omitido")
})

test_that("interpretation labels follow the language and the AUC", {
  a <- prep(lang = "es")
  tab <- roc_biomarkers(a, "ckd", bio)
  expect_true(all(tab$interpretation %in% c("Sobresaliente", "Excelente", "Aceptable",
                                            "Insatisfactoria", "Sin discriminación")))
  expect_equal(tab$interpretation, as.character(auc_class(tab$auc, "es")))
  en <- roc_biomarkers(prep(), "ckd", bio)
  expect_equal(en$interpretation, as.character(auc_class(en$auc, "en")))
  expect_equal(attr(en, "lang"), "en")
  expect_s3_class(roc_of(en, "ckd", "egfr"), "registat_roc")
  expect_null(roc_of(en, "ckd", "nothing"))
})
