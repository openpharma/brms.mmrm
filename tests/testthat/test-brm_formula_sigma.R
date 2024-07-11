test_that("brm_formula_sigma() with default names and all non-subgroup terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  expect_error(
    brm_formula_sigma(
      data = data,
      intercept = TRUE,
      baseline = TRUE,
      baseline_time = TRUE,
      group = TRUE,
      group_time = TRUE,
      time = TRUE,
      check_rank = TRUE
    ),
    class = "brm_error"
  )
  out <- brm_formula_sigma(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    check_rank = FALSE
  )
  expect_s3_class(out, "brms_mmrm_formula_sigma")
  expect_s3_class(out, "formula")
  expect_false(attr(out, "brm_allow_effect_size"))
  expect_equal(
    deparse(out, width.cutoff = 500L),
    "sigma ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT"
  )
})

test_that("brm_formula_sigma() same with homogeneous variance", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  out <- brm_formula_sigma(
    data = data,
    intercept = TRUE,
    baseline = FALSE,
    baseline_time = FALSE,
    group = FALSE,
    group_time = FALSE,
    time = FALSE,
    check_rank = TRUE
  )
  expect_s3_class(out, "brms_mmrm_formula_sigma")
  expect_s3_class(out, "formula")
  expect_true(attr(out, "brm_allow_effect_size"))
  expect_equal(deparse(out), "sigma ~ 1")
})

test_that("brm_formula_sigma() with default names and terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      subgroup = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    subgroup = "subgroup",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x"
  )
  out <- brm_formula_sigma(data = data)
  expect_s3_class(out, "brms_mmrm_formula_sigma")
  expect_s3_class(out, "formula")
  expect_true(attr(out, "brm_allow_effect_size"))
  expect_equal(deparse(out), "sigma ~ 0 + AVISIT")
})

test_that("brm_formula_sigma() with default names and terms, subgroup", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      subgroup = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    subgroup = "subgroup",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x"
  )
  out <- brm_formula_sigma(
    data = data,
    time = FALSE,
    subgroup = TRUE
  )
  expect_s3_class(out, "brms_mmrm_formula_sigma")
  expect_s3_class(out, "formula")
  expect_true(attr(out, "brm_allow_effect_size"))
  expect_equal(deparse(out), "sigma ~ 0 + subgroup")
})

test_that("brm_formula_sigma() brm_allow_effect_size", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      subgroup = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    subgroup = "subgroup",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x"
  )
  out <- brm_formula_sigma(
    data = data,
    time = FALSE,
    subgroup = TRUE,
    covariates = TRUE
  )
  expect_s3_class(out, "brms_mmrm_formula_sigma")
  expect_s3_class(out, "formula")
  expect_false(attr(out, "brm_allow_effect_size"))
})
