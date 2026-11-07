dict <- load_template("t2d")
prep <- function(n = 228, seed = 3, lang = "en") analysis_prepare(demo_t2d(n, seed), dict, lang)

test_that("binary_variables lists yes/no variables and the 'at least one' flags", {
  a <- prep()
  b <- binary_variables(a)
  expect_true(all(c("ckd", "hypertension", "dialysis", "any_complication") %in% b))
  expect_false("sex_at_birth" %in% b)
  expect_false("bmi_category" %in% b)
  expect_equal(names(b)[b == "ckd"], "Chronic kidney disease")
})

test_that("normality_table returns NA for too few or constant values", {
  a <- prep()
  nt <- normality_table(a, c("age_years", "hba1c_pct"))
  expect_equal(nt$p_value[1], stats::shapiro.test(a$data$age_years)$p.value)
  expect_equal(nt$normal, nt$p_value > 0.05)
  d <- demo_t2d(30, 1)
  d$heart_rate_bpm <- 70L
  d$respiratory_rate_rpm <- c(16L, 17L, rep(NA, 28))
  a2 <- analysis_prepare(d, dict, "en")
  nt2 <- normality_table(a2, c("heart_rate_bpm", "respiratory_rate_rpm"))
  expect_true(all(is.na(nt2$p_value)))
  expect_true(all(is.na(nt2$normal)))
  expect_error(normality_table(a, "sex_at_birth"), "must be numeric")
})

test_that("compare_continuous picks the test by normality and matches base R", {
  a <- prep()
  tab <- compare_continuous(a, "ckd", c("age_years", "creatinine_mgdl", "hemoglobin_gdl"))
  expect_equal(nrow(tab), 3L)
  y <- binary_outcome(a, "ckd")
  for (v in tab$variable) {
    x <- a$data[[v]]
    isn <- isTRUE(normal_flag(x[!is.na(y)]))
    ref <- if (isn) stats::t.test(x ~ y)$p.value else suppressWarnings(stats::wilcox.test(x ~ y)$p.value)
    expect_equal(tab$p_value[tab$variable == v], ref, tolerance = 1e-10)
    expect_equal(tab$test[tab$variable == v],
                 ui_text(if (isn) "test_welch" else "test_wilcoxon", "en"))
  }
  expect_true(any(tab$test == ui_text("test_wilcoxon", "en")))
  expect_match(names(tab)[3], "^Overall \\(n=")
  expect_match(names(tab)[5], "^Yes \\(n=")
  expect_equal(attr(tab, "outcome"), "ckd")
})

test_that("compare_continuous formats median (Q1 - Q3) and mean (SD)", {
  a <- prep()
  x <- a$data$age_years
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  med <- compare_continuous(a, "ckd", "age_years", summary = "median", digits = 1)
  expect_equal(med[[3]], sprintf("%.1f (%.1f - %.1f)", q[2], q[1], q[3]))
  avg <- compare_continuous(a, "ckd", "age_years", summary = "mean", digits = 2)
  expect_equal(avg[[3]], sprintf("%.2f (%.2f)", mean(x), stats::sd(x)))
  expect_error(compare_continuous(a, "age_years", "bmi"), "not a yes/no")
  expect_false("n_complications" %in% compare_continuous(a, "ckd")$variable)
})

test_that("associations_categorical gives the Wald OR for a large 2x2 table", {
  d <- demo_t2d(60, 1)
  d$hypertension <- c(rep("yes", 20), rep("no", 20), rep("yes", 10), rep("no", 10))
  d$ckd <- c(rep("yes", 20), rep("no", 20), rep("no", 10), rep("yes", 10))
  # exposed (hypertension yes): 20 ckd yes, 10 ckd no; unexposed: 10 yes, 20 no
  a <- analysis_prepare(d, dict, "en")
  r <- associations_categorical(a, "ckd", "hypertension")
  expect_equal(r$test, ui_text("test_chisq", "en"))
  expect_equal(r$or, (20 * 20) / (10 * 10))
  ee <- sqrt(1 / 20 + 1 / 10 + 1 / 10 + 1 / 20)
  expect_equal(c(r$ci_low, r$ci_high), exp(log(4) + c(-1, 1) * 1.96 * ee))
  expect_equal(r$p_value, stats::chisq.test(matrix(c(20, 10, 10, 20), 2))$p.value)
  expect_equal(r$compared, "Yes / No")
  expect_equal(r$n, 60L)
})

test_that("associations_categorical uses Fisher when expected counts are small", {
  d <- demo_t2d(40, 1)
  d$hypertension <- c(rep("yes", 3), rep("no", 37))
  d$ckd <- c("yes", "yes", "no", "yes", rep("no", 36))
  a <- analysis_prepare(d, dict, "en")
  r <- associations_categorical(a, "ckd", "hypertension")
  tab <- table(factor(d$hypertension, c("no", "yes")), factor(d$ckd, c("no", "yes")))
  f <- stats::fisher.test(tab)
  expect_equal(r$test, ui_text("test_fisher", "en"))
  expect_equal(r$or, unname(f$estimate))
  expect_equal(c(r$ci_low, r$ci_high), as.numeric(f$conf.int))
  expect_equal(r$p_value, f$p.value)
})

test_that("associations_categorical leaves 'unknown' out and handles k-level tables", {
  d <- demo_t2d(100, 2)
  d$hypertension[1:10] <- "unknown"
  a <- analysis_prepare(d, dict, "en")
  r <- associations_categorical(a, "ckd", "hypertension")
  ok <- d$hypertension != "unknown" & d$ckd != "unknown"
  expect_equal(r$n, sum(ok))
  r2 <- associations_categorical(a, "ckd", "bmi_category")
  expect_true(is.na(r2$or))
  expect_equal(r2$compared, "")
  # default exposures: no outcome, no calculated flags
  all <- associations_categorical(a, "ckd")
  expect_false("ckd" %in% all$exposure)
  expect_false(any(all$exposure %in% aggregate_vars()))
  expect_true(all(c("exposure_label", "or", "p_value") %in% names(all)))
})

test_that("empty associations keep their columns", {
  a <- prep()
  r <- associations_categorical(a, "ckd", "ckd")
  expect_equal(nrow(r), 0L)
  expect_equal(names(r), c("outcome", "outcome_label", "exposure", "exposure_label",
                           "compared", "n", "test", "or", "ci_low", "ci_high", "p_value"))
})

test_that("simulated Fisher is reproducible and leaves the RNG alone", {
  tab <- matrix(c(3, 1, 0, 2, 4, 1, 0, 1, 5), 3)
  p1 <- one_association(tab)$p
  p2 <- one_association(tab)$p
  expect_identical(p1, p2)
  expect_equal(one_association(tab)$test, "test_fisher")
  set.seed(5); a1 <- stats::runif(1)
  set.seed(5); invisible(one_association(tab)); a2 <- stats::runif(1)
  expect_equal(a1, a2)
})

test_that("correlations match cor.test and adjust over all pairs", {
  a <- prep()
  vars <- c("age_years", "hba1c_pct", "fasting_glucose_mgdl", "egfr")
  r <- correlations(a, vars)
  expect_equal(nrow(r), 6L)
  expect_equal(r$p_adj, stats::p.adjust(r$p_value, "holm"))
  expect_true(all(diff(abs(r$r)) <= 1e-12))
  row <- r[r$var1 == "hba1c_pct" & r$var2 == "fasting_glucose_mgdl", ]
  ct <- suppressWarnings(stats::cor.test(a$data$hba1c_pct, a$data$fasting_glucose_mgdl,
                                          method = "spearman", exact = FALSE))
  expect_equal(row$r, unname(ct$estimate))
  expect_equal(row$p_value, ct$p.value)
  expect_equal(row$method, "spearman")
  rp <- correlations(a, vars, "pearson", adjust = "bonferroni")
  row <- rp[rp$var1 == "age_years" & rp$var2 == "hba1c_pct", ]
  expect_equal(row$r, unname(stats::cor.test(a$data$age_years, a$data$hba1c_pct)$estimate))
  expect_equal(attr(rp, "adjust"), "bonferroni")
  ra <- correlations(a, vars, "auto")
  expect_true(all(ra$method %in% c("pearson", "spearman")))
  expect_error(correlations(a, "age_years"), "at least two")
})

test_that("prevalence_table counts, intervals and groups", {
  d <- demo_t2d(100, 3)
  d$ckd[1:5] <- "unknown"
  a <- analysis_prepare(d, dict, "en")
  p <- prevalence_table(a, "ckd", by = "sex_at_birth")
  expect_equal(as.character(p$group), c("Overall", "Female", "Male"))
  ov <- p[p$group == "Overall", ]
  n_yes <- sum(d$ckd == "yes"); N <- sum(d$ckd != "unknown")
  expect_equal(c(ov$n, ov$N), c(n_yes, N))
  expect_equal(ov$pct, 100 * n_yes / N)
  expect_equal(c(ov$ci_low, ov$ci_high), 100 * as.numeric(stats::binom.test(n_yes, N)$conf.int))
  f <- p[p$group == "Female", ]
  keep <- d$sex_at_birth == "female" & d$ckd != "unknown"
  expect_equal(f$N, sum(keep))
  expect_equal(f$n, sum(keep & d$ckd == "yes"))
  expect_equal(sum(p$N[p$group != "Overall"]),
               sum(d$ckd != "unknown" & d$sex_at_birth %in% c("female", "male")))
  all <- prevalence_table(a)
  expect_false(any(all$variable %in% aggregate_vars()))
})

test_that("outlier_summary flags values beyond k interquartile ranges", {
  a <- analysis_prepare(demo_t2d(120, seed = 3), load_template("t2d"), "en")
  a$data$age_years[c(5, 9)] <- c(400, -300)
  o <- outlier_summary(a, vars = c("age_years", "weight_kg"))
  expect_equal(o$variable[1], "age_years")
  expect_equal(o$n_outliers[o$variable == "age_years"], 2L)
  expect_equal(o$rows[o$variable == "age_years"], "5, 9")
  y <- a$data$age_years
  q <- stats::quantile(y, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  expect_equal(o$upper[o$variable == "age_years"], q[2] + 3 * (q[2] - q[1]))
  # a wider rule flags no more values than a narrower one
  expect_lte(sum(outlier_summary(a, k = 5)$n_outliers), sum(outlier_summary(a, k = 1.5)$n_outliers))
  expect_error(outlier_summary(a, k = 0), "positive")
  expect_error(outlier_summary(a, vars = "ckd"), "numeric")
})

test_that("plot_relation can draw a loess curve and reports Spearman r", {
  a <- analysis_prepare(demo_t2d(100, seed = 3), load_template("t2d"), "en")
  p <- plot_relation(a, "creatinine_mgdl", "egfr", color = "ckd", smooth = "loess")
  expect_s3_class(p, "ggplot")
  r <- stats::cor(a$data$creatinine_mgdl, a$data$egfr, method = "spearman",
                  use = "complete.obs")
  expect_match(p$labels$caption, formatC(r, format = "f", digits = 3), fixed = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_error(plot_relation(a, "creatinine_mgdl", "egfr", smooth = "spline"))
})
