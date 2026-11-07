test_that("roc_curve matches pROC (AUC, DeLong CI, Youden point)", {
  skip_if_not_installed("pROC")
  d <- demo_t2d(228, seed = 3)
  y <- as.integer(d$ckd == "yes")
  for (dir in c("<", ">")) {
    x <- if (dir == "<") d$creatinine_mgdl else d$hemoglobin_gdl
    ok <- !is.na(x)
    mine <- roc_curve(y, x, "auto")
    ref <- pROC::roc(y[ok], x[ok], quiet = TRUE, direction = "auto", ci = TRUE)
    expect_equal(mine$direction, dir)
    expect_equal(mine$auc, as.numeric(pROC::auc(ref)), tolerance = 1e-10)
    ci <- as.numeric(pROC::ci(ref))
    expect_equal(c(mine$ci_low, mine$ci_high), ci[c(1, 3)], tolerance = 1e-8)
    co <- pROC::coords(ref, "best", best.method = "youden",
                       ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"))
    co <- lapply(as.list(co), function(v) unlist(v)[1])
    expect_equal(mine$best$threshold, co$threshold, tolerance = 1e-8)
    expect_equal(mine$best$sensitivity, co$sensitivity, tolerance = 1e-8)
    expect_equal(mine$best$specificity, co$specificity, tolerance = 1e-8)
    expect_equal(mine$best$ppv, co$ppv, tolerance = 1e-8)
    expect_equal(mine$best$npv, co$npv, tolerance = 1e-8)
  }
})

test_that("roc_curve handles ties, perfect separation and bad input", {
  skip_if_not_installed("pROC")
  y <- c(0, 0, 0, 1, 1, 1, 0, 1)
  x <- c(1, 2, 2, 2, 3, 3, 1, 3)
  ref <- pROC::roc(y, x, quiet = TRUE, direction = "<")
  expect_equal(roc_curve(y, x, "<")$auc, as.numeric(pROC::auc(ref)))
  perfect <- roc_curve(c(0, 0, 0, 1, 1), c(1, 2, 3, 10, 11), "<")
  expect_equal(perfect$auc, 1)
  expect_equal(perfect$best$sensitivity, 1)
  expect_equal(perfect$best$specificity, 1)
  expect_s3_class(perfect, "registat_roc")
  expect_error(roc_curve(c(1, 1, 1), c(1, 2, 3)), "both events")
  expect_error(roc_curve(c(0, 2, 1), c(1, 2, 3)), "only 0 and 1")
  # missing pairs are dropped
  r <- roc_curve(c(0, 0, 1, 1, NA, 1), c(1, 2, 3, 4, 5, NA), "<")
  expect_equal(r$n_cases + r$n_controls, 4L)
})

test_that("auc_class uses the 0.9 / 0.8 / 0.7 / 0.6 boundaries", {
  v <- c(0.95, 0.9, 0.899, 0.8, 0.799, 0.7, 0.699, 0.6, 0.599, 0.5, NA)
  en <- as.character(auc_class(v, "en"))
  expect_equal(en, c("Outstanding", "Outstanding", "Excellent", "Excellent",
                     "Acceptable", "Acceptable", "Unsatisfactory", "Unsatisfactory",
                     "No discrimination", "No discrimination", NA))
  es <- as.character(auc_class(c(0.95, 0.55), "es"))
  expect_equal(es, c("Sobresaliente", "Sin discriminación"))
  expect_equal(nlevels(auc_class(0.5)), 5L)
})

test_that("hosmer_lemeshow matches a hand computation", {
  set.seed(10)
  p <- stats::runif(200, 0.05, 0.95)
  y <- stats::rbinom(200, 1, p)
  q <- stats::quantile(p, seq(0, 1, 0.1), names = FALSE)
  g <- cut(p, q, include.lowest = TRUE)
  stat <- 0
  for (k in levels(g)) {
    i <- g == k
    o1 <- sum(y[i]); e1 <- sum(p[i]); n <- sum(i)
    stat <- stat + (o1 - e1)^2 / e1 + ((n - o1) - (n - e1))^2 / (n - e1)
  }
  h <- hosmer_lemeshow(y, p, 10)
  expect_equal(h$statistic, stat, tolerance = 1e-10)
  expect_equal(h$df, 8L)
  expect_equal(h$p_value, stats::pchisq(stat, 8, lower.tail = FALSE))
  # fewer than three groups
  h2 <- hosmer_lemeshow(c(0, 1, 0, 1), c(0.5, 0.5, 0.5, 0.5), 10)
  expect_true(is.na(h2$p_value))
})

test_that("nagelkerke_r2 follows the textbook formula", {
  set.seed(2)
  x <- stats::rnorm(150)
  y <- stats::rbinom(150, 1, stats::plogis(0.8 * x))
  m <- stats::glm(y ~ x, family = stats::binomial())
  m0 <- stats::glm(y ~ 1, family = stats::binomial())
  n <- 150
  ll0 <- as.numeric(stats::logLik(m0)); ll1 <- as.numeric(stats::logLik(m))
  cox <- 1 - exp((2 / n) * (ll0 - ll1))
  expect_equal(nagelkerke_r2(m), cox / (1 - exp((2 / n) * ll0)),
               tolerance = 1e-10)
})

test_that("vif_values matches car::vif, with numeric and factor terms", {
  skip_if_not_installed("car")
  set.seed(3)
  n <- 300
  d <- data.frame(a = stats::rnorm(n), g = factor(sample(c("u", "v", "w"), n, TRUE)))
  d$b <- 0.6 * d$a + stats::rnorm(n)
  d$y <- stats::rbinom(n, 1, stats::plogis(0.5 * d$a - 0.4 * d$b))
  m1 <- stats::glm(y ~ a + b, data = d, family = stats::binomial())
  expect_equal(unname(vif_values(m1)), unname(car::vif(m1)), tolerance = 1e-8)
  m2 <- stats::glm(y ~ a + b + g, data = d, family = stats::binomial())
  cv <- car::vif(m2)
  ref <- cv[, "GVIF"]^(1 / cv[, "Df"])
  expect_equal(unname(vif_values(m2)), unname(ref), tolerance = 1e-8)
  expect_equal(names(vif_values(m2)), rownames(cv))
  m3 <- stats::glm(y ~ a, data = d, family = stats::binomial())
  expect_null(vif_values(m3))
})

test_that("proportion_ci is the Clopper-Pearson interval", {
  expect_equal(proportion_ci(12, 40),
               as.numeric(stats::binom.test(12, 40)$conf.int))
  expect_equal(proportion_ci(0, 10, 0.9),
               as.numeric(stats::binom.test(0, 10, conf.level = 0.9)$conf.int))
  expect_true(all(is.na(proportion_ci(1, 0))))
})
