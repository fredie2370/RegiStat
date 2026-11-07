test_that("demo_t2d is reproducible and leaves the global RNG alone", {
  expect_identical(demo_t2d(60, seed = 4), demo_t2d(60, seed = 4))
  expect_false(identical(demo_t2d(60, seed = 4), demo_t2d(60, seed = 5)))
  set.seed(99); before <- stats::runif(1)
  set.seed(99); invisible(demo_t2d(60, seed = 4)); after <- stats::runif(1)
  expect_equal(before, after)
})

test_that("demo_t2d data pass validation for several seeds and sizes", {
  dict <- load_template("t2d")
  for (n in c(20, 60, 228, 500)) {
    for (s in c(1, 2026)) {
      d <- demo_t2d(n, seed = s)
      expect_equal(nrow(d), n)
      expect_equal(nrow(validate_data(d, dict)), 0L)
    }
  }
})

test_that("demo_t2d has DEMO ids, events and linked complications", {
  d <- demo_t2d(228, seed = 2026)
  expect_true(all(grepl("^DEMO-[0-9]{4}$", d$record_id)))
  expect_false(anyDuplicated(d$record_id) > 0)
  expect_gte(sum(d$ckd == "yes"), 10)
  expect_gte(sum(d$dialysis == "yes"), 10)
  expect_gte(sum(d$hypertension == "yes"), 10)
  # dialysis almost only among patients with chronic kidney disease
  expect_lte(sum(d$dialysis == "yes" & d$ckd == "no"), 0.1 * sum(d$dialysis == "yes"))
  expect_true(any(is.na(d$uric_acid_mgdl)))
  expect_equal(names(d), names(synthetic_t2d(20, seed = 1)$data))
})

test_that("demo_t2d checks n", {
  expect_error(demo_t2d(19), "at least 20")
  expect_error(demo_t2d(NA), "at least 20")
})
