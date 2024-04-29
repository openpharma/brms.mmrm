test_that("brm_prior_simple()", {
  set.seed(0L)
  data <- brm_simulate_outline()
  data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE,
    check_rank = FALSE
  )
  out <- brm_prior_simple(
    data = data,
    formula = formula,
    intercept = "normal(0, 1)",
    coefficients = "normal(0, 2)",
    sigma = "normal(0, 3)",
    correlation = "lkj(2.5)"
  )
  expect_equal(out$prior[out$class == "Intercept"], "normal(0, 1)")
  expect_equal(out$prior[out$class == "b" & out$dpar == ""], "normal(0, 2)")
  expect_equal(
    out$prior[out$class == "b" & out$dpar == "sigma"],
    "normal(0, 3)"
  )
  expect_equal(out$prior[out$class == "cortime"], "lkj(2.5)")
})
