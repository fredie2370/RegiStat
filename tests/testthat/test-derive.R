
test_that("derived flags work for a single row and for no rows", {
  one <- data.frame(retinopathy = "yes", ckd = "no", hypertension = "yes")
  r <- derive_variables(one)
  expect_equal(r$n_complications, 1L)
  expect_equal(r$n_comp_comorb, 2L)
  zero <- derive_variables(one[0, ])
  expect_equal(nrow(zero), 0L)
})
