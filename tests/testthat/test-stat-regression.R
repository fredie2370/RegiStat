dict <- load_template("t2d")
prep <- function(n = 228, seed = 3, lang = "en") analysis_prepare(demo_t2d(n, seed), dict, lang)
preds <- c("age_years", "sex_at_birth", "systolic_bp_mmhg", "hypertension",
           "fasting_glucose_mgdl", "hemoglobin_gdl", "uric_acid_mgdl", "tyg")

test_that("logistic_model equals a direct glm without selection", {
  a <- prep()
  m <- logistic_model(a, "ckd", c("age_years", "hemoglobin_gdl", "sex_at_birth"),
                      selection = "none")
  expect_s3_class(m, "registat_model")
  d <- a$data
  y <- binary_outcome(a, "ckd")
  dd <- data.frame(y = y, age = d$age_years, hb = d$hemoglobin_gdl,
                   sex = droplevels(d$sex_at_birth))
  dd <- dd[stats::complete.cases(dd), ]
  ref <- stats::glm(y ~ age + hb + sex, data = dd, family = stats::binomial())
  expect_equal(unname(stats::coef(m$model)), unname(stats::coef(ref)), tolerance = 1e-8)
  expect_equal(m$n, nrow(dd))
  expect_equal(m$terms$or, unname(exp(stats::coef(ref))[-1]), tolerance = 1e-8)
  expect_equal(m$terms$p_value, summary(ref)$coefficients[-1, 4], tolerance = 1e-8,
               ignore_attr = TRUE)
  expect_true(all(m$terms$ci_low < m$terms$or & m$terms$or < m$terms$ci_high))
  expect_output(print.registat_model(m), "logistic model")
})

test_that("backward selection equals stats::step", {
  a <- prep()
  m <- logistic_model(a, "ckd", preds, selection = "backward")
  full <- logistic_model(a, "ckd", preds, selection = "none")
  refit <- stats::glm(stats::reformulate(full$predictors, "outcome"), data = full$data,
                      family = stats::binomial())
  ref <- stats::step(refit, direction = "backward", trace = 0)
  expect_equal(sort(m$selected), sort(attr(stats::terms(ref), "term.labels")))
  expect_equal(unname(stats::coef(m$model)), unname(stats::coef(ref)), tolerance = 1e-8)
  expect_lte(stats::AIC(m$model), stats::AIC(full$model) + 1e-8)
})

test_that("unknown answers are missing and yes/no predictors use 'no' as reference", {
  d <- demo_t2d(228, 3)
  d$hypertension[1:20] <- "unknown"
  a <- analysis_prepare(d, dict, "en")
  m <- logistic_model(a, "ckd", c("hypertension", "age_years"), selection = "none")
  ok <- d$hypertension != "unknown" & d$ckd != "unknown"
  expect_equal(m$n, sum(ok))
  expect_equal(levels(m$data$hypertension), c("No", "Yes"))
  expect_true("Hypertension: Yes vs No" %in% m$terms$label)
  s <- logistic_model(a, "ckd", c("sex_at_birth", "age_years"), selection = "none")
  expect_true("Sex assigned at birth: Male vs Female" %in% s$terms$label)
})

test_that("model notes report EPV, constant predictors and sparse levels", {
  a <- prep()
  m <- logistic_model(a, "ckd", c("age_years", "hemoglobin_gdl"), selection = "none",
                      min_events = 10)
  expect_equal(length(m$notes), 0L)
  # small number of events per coefficient
  d <- demo_t2d(228, 3)
  keep <- which(d$ckd == "no")
  yes <- which(d$ckd == "yes")[1:12]
  d2 <- d[c(keep[1:60], yes), ]
  a2 <- analysis_prepare(d2, dict, "en")
  m2 <- logistic_model(a2, "ckd", c("age_years", "hemoglobin_gdl", "uric_acid_mgdl",
                                    "systolic_bp_mmhg"), selection = "none")
  expect_true(any(grepl("events per estimated coefficient", m2$notes)))
  # constant predictor
  d3 <- demo_t2d(228, 3)
  d3$heart_rate_bpm <- 70L
  a3 <- analysis_prepare(d3, dict, "en")
  m3 <- logistic_model(a3, "ckd", c("age_years", "heart_rate_bpm"), selection = "none")
  expect_true(any(grepl("do not vary", m3$notes)))
  expect_false("heart_rate_bpm" %in% m3$predictors)
  # sparse level
  d4 <- demo_t2d(228, 3)
  d4$sex_at_birth[1:2] <- "intersex"
  a4 <- analysis_prepare(d4, dict, "en")
  m4 <- logistic_model(a4, "ckd", c("age_years", "sex_at_birth"), selection = "none")
  expect_true(any(grepl("fewer than 5 records", m4$notes)))
  expect_error(logistic_model(a, "ckd", character()), "at least one predictor")
})

test_that("logistic_model needs enough events", {
  a <- prep(60, 1)
  d <- a$data
  rare <- demo_t2d(60, 1)
  rare$amputation <- c("yes", "yes", "yes", rep("no", 57))
  ar <- analysis_prepare(rare, dict, "en")
  expect_error(logistic_model(ar, "amputation", "age_years"), "Too few records")
})

test_that("model_table joins crude and adjusted odds ratios", {
  a <- prep()
  m <- logistic_model(a, "ckd", preds)
  tb <- model_table(m)
  expect_equal(names(tb), c("variable", "label", "crude_or", "crude_ci", "crude_p",
                            "adj_or", "adj_ci", "adj_p"))
  expect_equal(nrow(tb), length(unique(c(m$crude$label, m$terms$label))))
  dropped <- setdiff(m$crude$label, m$terms$label)
  if (length(dropped)) {
    expect_true(all(is.na(tb$adj_or[tb$label %in% dropped])))
    expect_true(all(tb$adj_ci[tb$label %in% dropped] == ""))
  }
  kept <- tb[tb$label %in% m$terms$label, ]
  expect_false(anyNA(kept$adj_or))
  # crude OR is a one-variable glm
  y <- binary_outcome(a, "ckd")
  g <- stats::glm(y ~ a$data$age_years, family = stats::binomial())
  expect_equal(tb$crude_or[tb$variable == "age_years"], unname(exp(stats::coef(g))[2]),
               tolerance = 1e-8)
  expect_error(model_table(list()), "logistic_model")
})

test_that("fit measures agree with pROC and the textbook formulas", {
  skip_if_not_installed("pROC")
  a <- prep()
  m <- logistic_model(a, "ckd", preds)
  r <- pROC::roc(m$data$outcome, m$p_hat, quiet = TRUE, ci = TRUE)
  expect_equal(m$fit$auc, as.numeric(pROC::auc(r)), tolerance = 1e-10)
  expect_equal(c(m$fit$auc_low, m$fit$auc_high), as.numeric(pROC::ci(r))[c(1, 3)],
               tolerance = 1e-8)
  expect_equal(m$fit$nagelkerke, nagelkerke_r2(m$model))
  expect_equal(m$fit$hl_p, hosmer_lemeshow(m$data$outcome, m$p_hat)$p_value)
  expect_equal(m$fit$aic, stats::AIC(m$model))
  expect_equal(m$fit$n, m$n)
  expect_equal(m$fit$events, m$events)
  expect_equal(m$epv, m$events / (length(stats::coef(m$model)) - 1L))
})

test_that("regression_matrix collects models, skips rare outcomes", {
  d <- demo_t2d(228, 3)
  d$amputation <- c("yes", "yes", rep("no", 226))
  a <- analysis_prepare(d, dict, "en")
  rm <- regression_matrix(a, c("ckd", "dialysis", "amputation"),
                          c("age_years", "hemoglobin_gdl", "uric_acid_mgdl"))
  expect_s3_class(rm, "registat_models")
  expect_equal(names(rm$models), c("ckd", "dialysis"))
  expect_equal(rm$skipped$outcome, "amputation")
  expect_match(rm$skipped$reason, "Too few records")
  expect_equal(rm$summary$outcome, c("ckd", "dialysis"))
  expect_true(all(c("selected", "n", "events", "nagelkerke", "hl_p", "auc") %in% names(rm$summary)))
  m <- rm$matrix
  expect_equal(m$direction, ifelse(m$or > 1, "risk", "protective"))
  expect_equal(m$significant, m$p_value < 0.05)
  expect_true(all(m$outcome %in% c("ckd", "dialysis")))
  expect_equal(nrow(m), sum(vapply(rm$models, function(x) nrow(x$terms), integer(1))))
})

test_that("a model without predictors has empty terms and plots fail clearly", {
  a <- prep(228, 3)
  set.seed(1)
  d <- a$data
  d$noise1 <- stats::rnorm(nrow(d))
  m <- logistic_model(a, "retinopathy", "heart_rate_bpm", selection = "backward")
  if (length(m$selected) == 0L) {
    expect_equal(nrow(m$terms), 0L)
    expect_true(is.null(m$roc))
    expect_true(any(grepl("removed every predictor", m$notes)))
    expect_error(plot_forest(m), "no predictors")
    expect_error(plot_model_roc(m), "no predictors")
  } else {
    succeed()
  }
  # force an empty model deterministically with pure noise
  dd <- demo_t2d(228, 5)
  dd$heart_rate_bpm <- withr::with_seed(7, as.integer(round(stats::rnorm(228, 78, 10))))
  dd$ckd <- withr::with_seed(8, sample(c("yes", "no"), 228, TRUE))
  ae <- analysis_prepare(dd, dict, "en")
  me <- logistic_model(ae, "ckd", "heart_rate_bpm")
  if (length(me$selected) == 0L) {
    expect_equal(nrow(me$terms), 0L)
    expect_error(plot_forest(me), "no predictors")
    expect_error(plot_model_roc(me), "no predictors")
  }
  expect_error(plot_forest(list()), "logistic_model")
  expect_error(plot_predictor_matrix(list()), "regression_matrix")
  empty <- regression_matrix(ae, "ckd", "heart_rate_bpm")
  if (nrow(empty$matrix) == 0L) expect_error(plot_predictor_matrix(empty), "No model kept")
})

test_that("odds-ratio rows keep their variable and label with accented Spanish levels", {
  a <- analysis_prepare(demo_t2d(150, seed = 7), load_template("t2d"), "es")
  m <- logistic_model(a, "ckd", c("age_years", "hypertension", "fasting_glucose_mgdl"),
                      selection = "none")
  expect_false(anyNA(m$crude$variable))
  expect_false(anyNA(m$terms$variable))
  expect_equal(m$terms$variable, c("age_years", "hypertension", "fasting_glucose_mgdl"))
  tb <- model_table(m)
  expect_false(anyNA(tb$variable))
  expect_false(anyNA(tb$crude_or))
  expect_false(anyNA(tb$adj_or))
  expect_equal(nrow(tb), 3L)
})
