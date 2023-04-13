test_that("brm_formula() with default names and all terms", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_s3_class(out, "formula")
  out <- deparse(out, width.cutoff = 500L)
  expect_equal(
    out,
    paste(
      "CHG ~ AVISIT + BASE + BASE:AVISIT + TRT01P + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() with all user-supplied columns and all terms", {
  out <- brm_formula(
    response = "y",
    group = "g",
    time = "t",
    base = "b",
    patient = "p",
    covariates = c("a", "b"),
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    "y ~ t + b + b:t + g + g:t + a + b + brms::unstr(time = t, gr = p)"
  )
})

test_that("brm_formula() without intercept", {
  out <- brm_formula(
    intercept = FALSE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ 0 + AVISIT + BASE + BASE:AVISIT + TRT01P + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() without group effect", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = FALSE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ AVISIT + BASE + BASE:AVISIT + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() without time effect", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = FALSE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ BASE + BASE:AVISIT + TRT01P + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() without baseline effect", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = FALSE,
    interaction_base = TRUE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ AVISIT + BASE:AVISIT + TRT01P + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() without baseline interaction", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = FALSE,
    interaction_group = TRUE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ AVISIT + BASE + TRT01P + TRT01P:AVISIT",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() without group interaction", {
  out <- brm_formula(
    intercept = TRUE,
    effect_group = TRUE,
    effect_time = TRUE,
    effect_base = TRUE,
    interaction_base = TRUE,
    interaction_group = FALSE
  )
  expect_equal(
    deparse(out, width.cutoff = 500L),
    paste(
      "CHG ~ AVISIT + BASE + BASE:AVISIT + TRT01P",
      "+ brms::unstr(time = AVISIT, gr = USUBJID)"
    )
  )
})
