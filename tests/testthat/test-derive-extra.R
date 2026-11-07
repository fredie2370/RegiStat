dict <- load_template("t2d")

thesis_tfg <- function(cr, age, sex) {
  e <- ifelse(sex == "female",
              ifelse(cr <= 0.7, 144 * (cr / 0.7)^(-0.329) * 0.993^age,
                     144 * (cr / 0.7)^(-1.209) * 0.993^age),
              ifelse(cr <= 0.9, 141 * (cr / 0.9)^(-0.411) * 0.993^age,
                     141 * (cr / 0.9)^(-1.209) * 0.993^age))
  round(e, 1)
}

grid <- function() {
  g <- expand.grid(creatinine_mgdl = c(0.4, 0.6, 0.7, 0.75, 0.9, 1.0, 1.4, 2.5, 6),
                   age_years = c(20, 45, 60, 85),
                   sex_at_birth = c("female", "male"), stringsAsFactors = FALSE)
  g
}

test_that("egfr_equation = '2009' equals the thesis formula", {
  g <- grid()
  d <- derive_variables(g, egfr_equation = "2009")
  expect_equal(d$egfr, thesis_tfg(g$creatinine_mgdl, g$age_years, g$sex_at_birth))
})

test_that("the 2021 equation is unchanged and rounded to whole numbers", {
  g <- grid()
  d <- derive_variables(g)
  expect_equal(d$egfr, derive_variables(g, egfr_equation = "2021")$egfr)
  expect_true(all(d$egfr == round(d$egfr)))
  f <- g$sex_at_birth == "female"
  k <- ifelse(f, 0.7, 0.9); al <- ifelse(f, -0.241, -0.302)
  ref <- round(142 * pmin(g$creatinine_mgdl / k, 1)^al * pmax(g$creatinine_mgdl / k, 1)^-1.2 *
                 0.9938^g$age_years * ifelse(f, 1.012, 1), 0)
  expect_equal(d$egfr, ref)
  # known value: woman 60 y, creatinine 0.9 mg/dL
  one <- derive_variables(data.frame(creatinine_mgdl = 0.9, age_years = 60,
                                     sex_at_birth = "female"))
  expect_equal(one$egfr, 73)
  expect_false(identical(derive_variables(g, egfr_equation = "2009")$egfr, d$egfr))
  expect_error(derive_variables(g, egfr_equation = "2000"))
  other <- derive_variables(data.frame(creatinine_mgdl = 1, age_years = 50,
                                       sex_at_birth = c("unknown", "intersex", NA)), egfr_equation = "2009")
  expect_true(all(is.na(other$egfr)))
})

test_that("tyg is ln(triglycerides x glucose / 2) with three decimals", {
  d <- derive_variables(data.frame(triglycerides_mgdl = c(150, 300, NA),
                                   fasting_glucose_mgdl = c(100, 180, 90)))
  expect_equal(d$tyg, c(round(log(150 * 100 / 2), 3), round(log(300 * 180 / 2), 3), NA))
  expect_false("tyg" %in% names(derive_variables(data.frame(triglycerides_mgdl = 1))))
})

test_that("complication and comorbidity counts treat unknown and NA as absent", {
  d <- data.frame(
    retinopathy = c("yes", "no", "unknown", "yes", NA),
    heart_failure = c("no", "no", "unknown", "yes", NA),
    ckd = c("yes", "no", "no", "yes", "yes"),
    dialysis = c("no", "no", "unknown", "no", "no"),
    amputation = c("no", "no", "no", "yes", "no"),
    hypertension = c("yes", "yes", "unknown", "no", NA),
    copd = c("no", "unknown", "yes", "yes", "no"), stringsAsFactors = FALSE)
  r <- derive_variables(d, "en")
  expect_equal(r$n_complications, c(2L, 0L, 0L, 4L, 1L))
  expect_equal(r$n_comorbidities, c(1L, 1L, 1L, 1L, 0L))
  expect_equal(r$n_comp_comorb, r$n_complications + r$n_comorbidities)
  expect_equal(as.character(r$any_complication), c("Yes", "No", "No", "Yes", "Yes"))
  expect_equal(as.character(r$any_comorbidity), c("Yes", "Yes", "Yes", "Yes", "No"))
  expect_equal(levels(r$any_comp_comorb), c("No", "Yes"))
  es <- derive_variables(d, "es")
  expect_equal(levels(es$any_complication), c("No", "Sí"))
  # only the available variables are counted
  part <- derive_variables(data.frame(ckd = c("yes", "no")), "en")
  expect_equal(part$n_complications, c(1L, 0L))
  expect_false("n_comorbidities" %in% names(part))
})

test_that("analysis_prepare keeps codes, binary levels and equation labels consistent", {
  d <- demo_t2d(80, 2)
  a <- analysis_prepare(d, dict, "en", egfr_equation = "2009")
  expect_equal(a$egfr_equation, "2009")
  expect_match(a$labels[["egfr"]], "CKD-EPI 2009")
  expect_match(analysis_prepare(d, dict, "es")$labels[["egfr"]], "CKD-EPI 2021")
  expect_equal(a$binary$ckd, c(positive = "Yes", negative = "No"))
  es <- analysis_prepare(d, dict, "es")
  expect_equal(es$binary$ckd, c(positive = "Sí", negative = "No"))
  expect_equal(unname(a$codes$sex_at_birth[c("female", "male")]), c("Female", "Male"))
  expect_equal(names(a$codes$ckd), c("yes", "no", "unknown"))
  expect_true(all(c("any_complication", "any_comorbidity", "any_comp_comorb") %in% names(a$binary)))
  expect_false("n_complications" %in% names(a$binary))
  expect_false("sex_at_birth" %in% names(a$binary))
  expect_equal(a$types[["any_complication"]], "categorical")
  expect_equal(a$types[["n_complications"]], "numeric")
  # the codes of a calculated flag point at "no" and "yes"
  expect_equal(unname(a$codes$any_complication), levels(a$data$any_complication))
})
