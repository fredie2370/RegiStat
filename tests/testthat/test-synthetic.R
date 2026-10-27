test_that("synthetic data are reproducible and leave the global RNG alone", {
  expect_identical(synthetic_t2d(60, seed = 7), synthetic_t2d(60, seed = 7))
  expect_false(identical(synthetic_t2d(60, seed = 7)$data,
                         synthetic_t2d(60, seed = 8)$data))

  set.seed(99)
  expected <- stats::runif(1)
  set.seed(99)
  invisible(synthetic_t2d(60, seed = 1, error_rate = 0.2))
  expect_equal(stats::runif(1), expected)
})

test_that("synthetic data are clearly marked as synthetic", {
  sim <- synthetic_t2d(40, seed = 3)
  expect_true(all(startsWith(sim$data$record_id, "SYN-")))
  expect_equal(nrow(sim$truth), 0L)
  expect_equal(nrow(sim$data), 40L)
})

test_that("invalid arguments are rejected", {
  expect_error(synthetic_t2d(n = 3), "at least 10")
  expect_error(synthetic_t2d(error_rate = 0.9), "between 0 and 0.5")
})

test_that("the injected error count matches the requested rate", {
  sim <- synthetic_t2d(200, seed = 5, error_rate = 0.1)
  expect_equal(nrow(sim$truth), 20L)
  expect_false(1L %in% sim$truth$row)
  expect_equal(anyDuplicated(sim$truth$row), 0L)
})
