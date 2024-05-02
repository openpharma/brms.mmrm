test_that("brm_prior_simple() unstructured", {
  set.seed(0L)
  data <- brm_simulate_outline()
  data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE,
    check_rank = FALSE
  )
  expect_warning(
    brm_prior_simple(
      data = data,
      formula = formula,
      intercept = "normal(0, 1)",
      coefficients = "normal(0, 2)",
      sigma = "normal(0, 3)",
      correlation = "lkj(2.5)"
    ),
    class = "brm_deprecate"
  )
  out <- brm_prior_simple(
    data = data,
    formula = formula,
    intercept = "normal(0, 1)",
    coefficients = "normal(0, 2)",
    sigma = "normal(0, 3)",
    unstructured = "lkj(2.5)"
  )
  expect_equal(out$prior[out$class == "Intercept"], "normal(0, 1)")
  expect_equal(out$prior[out$class == "b" & out$dpar == ""], "normal(0, 2)")
  expect_equal(
    out$prior[out$class == "b" & out$dpar == "sigma"],
    "normal(0, 3)"
  )
  expect_equal(out$prior[out$class == "cortime"], "lkj(2.5)")
  for (class in c("ar", "ma", "cosy")) {
    expect_equal(out$prior[out$class == class], character(0L))
  }
})

test_that("brm_prior_simple() arma", {
  set.seed(0L)
  data <- brm_simulate_outline()
  data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE,
    correlation = "autoregressive_moving_average",
    check_rank = FALSE
  )
  out <- brm_prior_simple(
    data = data,
    formula = formula,
    intercept = "normal(0, 1)",
    coefficients = "normal(0, 2)",
    sigma = "normal(0, 3)",
    autoregressive = "normal(1, 2)",
    moving_average = "normal(3, 4)"
  )
  expect_equal(out$prior[out$class == "Intercept"], "normal(0, 1)")
  expect_equal(out$prior[out$class == "b" & out$dpar == ""], "normal(0, 2)")
  expect_equal(
    out$prior[out$class == "b" & out$dpar == "sigma"],
    "normal(0, 3)"
  )
  expect_equal(out$prior[out$class == "ar"], "normal(1, 2)")
  expect_equal(out$prior[out$class == "ma"], "normal(3, 4)")
  for (class in c("cortime", "cosy")) {
    expect_equal(out$prior[out$class == class], character(0L))
  }
})

test_that("brm_prior_simple() cosy", {
  set.seed(0L)
  data <- brm_simulate_outline()
  data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE,
    correlation = "compound_symmetry",
    check_rank = FALSE
  )
  out <- brm_prior_simple(
    data = data,
    formula = formula,
    intercept = "normal(0, 1)",
    coefficients = "normal(0, 2)",
    sigma = "normal(0, 3)",
    compound_symmetry = "normal(1.5, 2)"
  )
  expect_equal(out$prior[out$class == "Intercept"], "normal(0, 1)")
  expect_equal(out$prior[out$class == "b" & out$dpar == ""], "normal(0, 2)")
  expect_equal(
    out$prior[out$class == "b" & out$dpar == "sigma"],
    "normal(0, 3)"
  )
  expect_equal(out$prior[out$class == "cosy"], "normal(1.5, 2)")
  for (class in c("ar", "ma", "cortime")) {
    expect_equal(out$prior[out$class == class], character(0L))
  }
})
