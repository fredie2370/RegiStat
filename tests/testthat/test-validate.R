dict <- load_template("t2d")

test_that("clean synthetic data produce no issues", {
  for (s in 1:5) {
    sim <- synthetic_t2d(300, seed = s)
    expect_equal(nrow(validate_data(sim$data, dict)), 0L)
  }
})

test_that("every injected error is found and nothing else is flagged", {
  sim <- synthetic_t2d(500, seed = 42, error_rate = 0.2)
  issues <- validate_data(sim$data, dict)
  key <- function(x) paste(x$row, x$rule)
  expect_setequal(key(issues), key(sim$truth))
  expect_equal(nrow(issues), nrow(sim$truth))
  # all six error types were exercised
  expect_setequal(
    unique(sim$truth$rule),
    c("required", "range", "choice", "type", "duplicate_id", "rule:bp_order")
  )
})

test_that("detection is perfect across seeds and error rates", {
  for (s in c(1, 11, 21)) {
    for (rate in c(0.05, 0.3)) {
      sim <- synthetic_t2d(250, seed = s, error_rate = rate)
      issues <- validate_data(sim$data, dict)
      key <- function(x) paste(x$row, x$rule)
      expect_setequal(key(issues), key(sim$truth))
    }
  }
})

test_that("messages follow the requested language", {
  sim <- synthetic_t2d(100, seed = 2, error_rate = 0.3)
  en <- validate_data(sim$data, dict, lang = "en")
  es <- validate_data(sim$data, dict, lang = "es")
  expect_equal(en$row, es$row)
  expect_true(any(grepl("Required value is missing", en$message)))
  expect_true(any(grepl("obligatorio", es$message)))
  expect_error(validate_data(sim$data, dict, lang = "fr"))
})

test_that("missing columns are reported once", {
  sim <- synthetic_t2d(30, seed = 4)
  d <- sim$data
  d$hba1c_pct <- NULL
  issues <- validate_data(d, dict)
  expect_equal(nrow(issues), 1L)
  expect_equal(issues$rule, "missing_column")
  expect_true(is.na(issues$row))
})

test_that("dates must be strict ISO and real calendar dates", {
  d <- synthetic_t2d(30, seed = 4)$data
  d$visit_date <- as.character(d$visit_date)
  d$visit_date[2] <- "2024-02-30"
  d$visit_date[3] <- "2024-1-5"
  d$visit_date[4] <- "2024-03-05abc"
  issues <- validate_data(d, dict)
  expect_setequal(issues$row, 2:4)
  expect_true(all(issues$rule == "type"))
})

test_that("whole-number and blank handling is correct", {
  d <- synthetic_t2d(30, seed = 4)$data
  d$age_years <- as.numeric(d$age_years)
  d$age_years[5] <- 40.5
  d$age_years[6] <- NA
  d$ckd[7] <- ""
  issues <- validate_data(d, dict)
  expect_setequal(paste(issues$row, issues$rule), c("5 type", "6 required"))
})

test_that("cross-field rules skip rows with missing inputs", {
  d <- synthetic_t2d(30, seed = 4)$data
  d$systolic_bp_mmhg[8] <- NA
  issues <- validate_data(d, dict)
  expect_equal(nrow(issues), 0L)
})

test_that("input types are checked", {
  expect_error(validate_data(list(), dict), "data frame")
  expect_error(validate_data(data.frame(), list()), "read_dictionary")
})
