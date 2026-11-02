dict <- load_template("t2d")

small <- function() {
  d <- synthetic_t2d(40, seed = 5)$data
  analysis_prepare(d, dict, "en")
}

test_that("analysis_prepare labels factors and adds calculated measures", {
  a <- small()
  expect_s3_class(a, "registat_analysis")
  expect_equal(a$types[["age_years"]], "numeric")
  expect_equal(a$types[["hypertension"]], "categorical")
  expect_equal(a$types[["bmi_category"]], "categorical")
  expect_equal(a$types[["egfr"]], "numeric")
  expect_false("record_id" %in% names(a$types))
  expect_false("visit_date" %in% names(a$types))
  expect_false("other_complications" %in% names(a$types))
  expect_equal(levels(a$data$sex_at_birth)[1:2], c("Female", "Male"))
  es <- analysis_prepare(synthetic_t2d(40, seed = 5)$data, dict, "es")
  expect_equal(levels(es$data$sex_at_birth)[1:2], c("Mujer", "Hombre"))
  expect_output(print(a), "records")
})

test_that("table_one matches hand calculations", {
  d <- data.frame(
    record_id = paste0("R", 1:6), visit_date = "2024-01-01",
    sex_at_birth = c("female", "female", "female", "male", "male", "male"),
    age_years = c(50, 60, 70, 40, 50, NA),
    hypertension = c("yes", "no", "yes", "no", "no", "yes"),
    stringsAsFactors = FALSE)
  dict2 <- validate_dictionary(list(variables = Filter(
    function(v) v$name %in% names(d), dict$variables)))
  a <- analysis_prepare(d, dict2, "en")
  t1 <- table_one(a)
  # mean of 50,60,70,40,50 = 54; SD = sqrt(((-4)^2+6^2+16^2+(-14)^2+(-4)^2)/4)
  sdv <- sqrt((16 + 36 + 256 + 196 + 16) / 4)
  row <- t1[t1$variable == "age_years" & t1$statistic == "Mean (SD)", ]
  expect_equal(row[[4]], sprintf("54.0 (%.1f)", sdv))
  med <- t1[t1$variable == "age_years" & t1$statistic == "Median [Q1, Q3]", ]
  expect_equal(med[[4]], "50.0 [50.0, 60.0]")
  expect_equal(t1[t1$variable == "age_years" & t1$statistic == "Missing", ][[4]], "1")
  yes <- t1[t1$variable == "hypertension" & t1$statistic == "Yes", ]
  expect_equal(yes[[4]], "3 (50.0%)")
  expect_true(grepl("(n=6)", names(t1)[4], fixed = TRUE))

  g <- table_one(a, by = "sex_at_birth")
  expect_equal(names(g)[4:6], c("Overall (n=6)", "Female (n=3)", "Male (n=3)"))
  expect_true("p" %in% names(g))
  expect_false("sex_at_birth" %in% g$variable)
  fem <- g[g$variable == "age_years" & g$statistic == "Mean (SD)", "Female (n=3)"]
  expect_equal(fem, "60.0 (10.0)")
})

test_that("group tests are the documented ones", {
  set.seed(1)
  x <- c(rnorm(30, 0), rnorm(30, 1))
  gr <- factor(rep(c("a", "b"), each = 30))
  expect_equal(RegiStat:::group_p_value(x, gr, "numeric"),
               stats::wilcox.test(x ~ gr, exact = FALSE)$p.value)
  g3 <- factor(rep(c("a", "b", "c"), each = 20))
  expect_equal(RegiStat:::group_p_value(x, g3, "numeric"),
               stats::kruskal.test(x ~ g3)$p.value)
  f <- factor(c(rep("y", 8), rep("n", 2), rep("y", 3), rep("n", 7)))
  g <- factor(rep(c("a", "b"), c(10, 10)))
  expect_equal(RegiStat:::group_p_value(f, g, "categorical"),
               stats::fisher.test(table(f, g))$p.value)
  expect_true(is.na(RegiStat:::group_p_value(f, factor(rep("a", 20)), "categorical")))
  expect_equal(RegiStat:::fmt_p(c(0.0004, 0.0456, NA)), c("<0.001", "0.046", ""))
})

test_that("table_one validates its arguments", {
  a <- small()
  expect_error(table_one(a, by = "age_years"), "categorical")
  expect_error(table_one(a, vars = "nope"), "Unknown analysis variable")
  expect_error(table_one(list()), "analysis_prepare")
})

test_that("missing_summary counts missing values", {
  a <- small()
  a$data$age_years[1:4] <- NA
  m <- missing_summary(a)
  expect_equal(m$variable[1], "age_years")
  expect_equal(m$n_missing[1], 4L)
  expect_equal(m$pct_missing[1], 10)
})

test_that("plots are ggplot objects and report dropped records", {
  a <- small()
  a$data$hba1c_pct[1:3] <- NA
  p <- plot_variable(a, "hba1c_pct")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "3 records")
  expect_s3_class(plot_variable(a, "hba1c_pct", by = "sex_at_birth"), "ggplot")
  expect_s3_class(plot_variable(a, "ckd"), "ggplot")
  expect_s3_class(plot_variable(a, "ckd", by = "hypertension"), "ggplot")
  expect_s3_class(plot_relation(a, "weight_kg", "bmi"), "ggplot")
  expect_s3_class(plot_relation(a, "weight_kg", "bmi", color = "sex_at_birth"), "ggplot")
  expect_error(plot_relation(a, "ckd", "bmi"), "numeric")
  # every plot builds without error
  for (pl in list(p, plot_relation(a, "age_years", "egfr"))) {
    expect_no_error(ggplot2::ggplot_build(pl))
  }
  expect_no_error(as_interactive(p))
})

test_that("tables and plots work in Spanish", {
  a <- analysis_prepare(synthetic_t2d(40, seed = 5)$data, dict, "es")
  t1 <- table_one(a, vars = "age_years")
  expect_equal(t1$statistic, c("Media (DE)", "Mediana [Q1, Q3]"))
  expect_match(names(t1)[4], "^Total")
})

test_that("registry_report renders a self-contained HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc not available")
  reg <- new_registry()
  registry_import(reg, synthetic_t2d(30, seed = 2)$data, user = "tester")
  out <- withr::local_tempfile(fileext = ".html")
  registry_report(reg, out, lang = "es", by = "sex_at_birth")
  html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(html, "Reporte del registro")
  expect_match(html, "Bitácora verificada")
  expect_match(html, "Mujer")
  expect_match(html, "Estad\u00edstico")
  expect_match(html, "Presi\u00f3n arterial sist\u00f3lica")
  expect_false(grepl("<U+", html, fixed = TRUE))
  expect_match(html, "data:image/png;base64")   # plots embedded, no network
  expect_false(grepl("(src|href)=[\"']https?://", html))

  expect_error(registry_report(reg, "x.txt"), "html")
  expect_error(registry_report(reg, out, by = "age_years"), "categorical")

  empty <- new_registry()
  out2 <- withr::local_tempfile(fileext = ".html")
  registry_report(empty, out2, lang = "en")
  expect_match(paste(readLines(out2, warn = FALSE), collapse = "\n"), "no records yet")
})
