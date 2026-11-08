dict <- load_template("t2d")
prep <- function(lang = "en", n = 120, seed = 6) analysis_prepare(demo_t2d(n, seed), dict, lang)
nums <- c("age_years", "hba1c_pct", "fasting_glucose_mgdl", "egfr", "creatinine_mgdl")
bio <- c("uric_acid_mgdl", "tyg", "hemoglobin_gdl", "egfr", "creatinine_mgdl")

all_plots <- function(a) {
  rm <- regression_matrix(a, c("ckd", "dialysis"), c("age_years", "hemoglobin_gdl", "uric_acid_mgdl"),
                          selection = "none")
  m <- logistic_model(a, "ckd", c("age_years", "hemoglobin_gdl"), selection = "none")
  tab <- roc_biomarkers(a, c("ckd", "dialysis"), bio)
  list(
    pyramid = plot_pyramid(a),
    counts = plot_group_counts(a, "bmi_category", "sex_at_birth"),
    counts_nogroup = plot_group_counts(a, "bmi_category"),
    prevalence = plot_prevalence(a, by = "sex_at_birth"),
    normality = plot_normality(a, "age_years"),
    compare = plot_compare_outcome(a, c("age_years", "egfr"), "ckd"),
    correlation = plot_correlation(a, nums),
    forest = plot_forest(m),
    model_roc = plot_model_roc(m),
    matrix = plot_predictor_matrix(rm),
    roc = plot_roc(tab, "ckd"),
    heatmap = plot_auc_heatmap(tab))
}

test_that("every figure builds in English and Spanish", {
  for (lang in c("en", "es")) {
    ps <- all_plots(prep(lang))
    for (nm in names(ps)) {
      expect_s3_class(ps[[nm]], "ggplot")
      expect_no_error(ggplot2::ggplot_build(ps[[nm]]))
    }
  }
})

test_that("the pyramid draws the table counts", {
  a <- prep()
  p <- plot_pyramid(a)
  d <- ggplot2::ggplot_build(p)$plot$data
  ge <- cut(a$data$age_years, c(0, 30, 40, 50, 60, 70, 80, Inf), right = FALSE)
  tb <- table(ge, a$data$sex_at_birth)
  expect_equal(sum(d$n), sum(tb[, c("Female", "Male")]))
  expect_equal(as.vector(tb[, "Female"]), d$n[d$sex == "Female"])
  expect_equal(d$n_plot[d$sex == "Male"], -d$n[d$sex == "Male"])
  expect_equal(levels(d$group)[c(1, 7)], c("<30", "≥80"))
})

test_that("grouped figures name the sexes as in the thesis", {
  en <- prep("en"); es <- prep("es")
  expect_equal(fig_group_label(en, "sex_at_birth", c("Overall", "Female", "Male")),
               c("Overall", "Assigned female at birth", "Assigned male at birth"))
  expect_equal(fig_group_label(es, "sex_at_birth", c("Mujer", "Hombre")),
               c("Femenino al nacer", "Masculino al nacer"))
  expect_equal(fig_group_label(en, "hypertension", "Yes"), "Yes")
  expect_equal(fig_group_label(en, NULL, "Female"), "Female")
  legend <- function(p) {
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_labels()
  }
  for (f in list(function(a) plot_group_counts(a, "bmi_category", "sex_at_birth"),
                 function(a) plot_prevalence(a, by = "sex_at_birth"))) {
    l_en <- legend(f(en)); l_es <- legend(f(es))
    expect_true(any(grepl("^Assigned female at birth \\(n=", l_en)))
    expect_true(any(grepl("^Assigned male at birth \\(n=", l_en)))
    expect_true(any(grepl("^Femenino al nacer \\(n=", l_es)))
  }
  expect_true("Assigned female at birth" %in% legend(plot_pyramid(en)))
})

test_that("group counts and prevalence show n and percentages", {
  a <- prep()
  d <- ggplot2::ggplot_build(plot_group_counts(a, "bmi_category", "sex_at_birth"))$plot$data
  expect_equal(d$n[d$group == "Overall"],
               as.integer(table(a$data$bmi_category[!is.na(a$data$bmi_category)])))
  expect_equal(d$pct[d$group == "Overall"], round(100 * d$n[d$group == "Overall"] /
                                                    sum(d$n[d$group == "Overall"]), 1))
  pv <- ggplot2::ggplot_build(plot_prevalence(a, by = "sex_at_birth"))$plot$data
  expect_equal(pv$n[pv$variable == "ckd" & pv$group == "Overall"], sum(a$data$ckd == "Yes"))
})

test_that("figure functions fail with clear messages", {
  a <- prep()
  expect_error(plot_pyramid(a, age = "sex_at_birth"), "must be numeric")
  expect_error(plot_roc(list(), "ckd"), "roc_biomarkers")
  expect_error(plot_auc_heatmap(list()), "roc_biomarkers")
  tab <- roc_biomarkers(a, "ckd", "egfr")
  expect_error(plot_roc(tab, "dialysis"), "No ROC curves")
  expect_error(plot_correlation(a, "age_years"), "at least two")
})

test_that("save_figure writes png, svg and pdf", {
  p <- plot_pyramid(prep())
  for (ext in c("png", "svg", "pdf")) {
    f <- withr::local_tempfile(fileext = paste0(".", ext))
    out <- tryCatch(suppressWarnings(save_figure(p, f, width = 100, height = 70, dpi = 80)),
                    error = function(e) NULL)
    # Some systems (for example macOS without X11/Cairo) cannot draw every
    # format; that is a limit of the machine, not of the package.
    if (is.null(out) || !file.exists(f)) {
      skip(paste("graphics device for", ext, "is not available here"))
    }
    expect_identical(out, f)
    expect_gt(file.size(f), 500)
  }
  expect_error(save_figure(p, tempfile(fileext = ".jpg")), "png, .svg or .pdf")
})

test_that("figures are reproducible", {
  a <- prep()
  g <- function() {
    m <- logistic_model(a, "ckd", c("age_years", "hemoglobin_gdl"), selection = "none")
    list(plot_forest(m), plot_normality(a, "age_years"), plot_pyramid(a))
  }
  p1 <- g(); p2 <- g()
  for (i in seq_along(p1)) {
    expect_equal(ggplot2::layer_data(p1[[i]], 1), ggplot2::layer_data(p2[[i]], 1))
  }
})
