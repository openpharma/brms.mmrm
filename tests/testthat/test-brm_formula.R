test_that("brm_formula() with default names and all non-subgroup terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = 1,
      AVISIT = "x",
      baseline = 2,
      TRT01P = "x",
      USUBJID = "x"
    ),
    outcome = "CHG",
    role = "change",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE
  )
  expect_s3_class(out, "brmsformula")
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() with default names and all terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = 1,
      AVISIT = "x",
      baseline = 2,
      TRT01P = "x",
      subgroup = "x",
      USUBJID = "x"
    ),
    outcome = "CHG",
    role = "change",
    group = "TRT01P",
    subgroup = "subgroup",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x"
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE
  )
  expect_s3_class(out, "brmsformula")
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:subgroup + baseline:subgroup:AVISIT",
      "+ baseline:AVISIT + TRT01P + TRT01P:subgroup + TRT01P:subgroup:AVISIT",
      "+ TRT01P:AVISIT + subgroup + subgroup:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() with all user-supplied columns, all non-sub terms", {
  data <- brm_data(
    data = tibble::tibble(
      y = 1,
      t = "x",
      b = 2,
      g = "x",
      p = "x",
      a = 1
    ),
    outcome = "y",
    role = "change",
    group = "g",
    time = "t",
    baseline = "b",
    patient = "p",
    covariates = c("a", "b"),
    reference_group = "x"
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    "y ~ b + b:t + g + g:t + t + a + b + unstr(time = t, gr = p)"
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + t"
    )
  )
})

test_that("brm_formula() with all user-supplied columns, all terms", {
  data <- brm_data(
    data = tibble::tibble(
      y = 1,
      t = "x",
      b = 2,
      g = "x",
      s = "x",
      p = "x",
      a = 1
    ),
    outcome = "y",
    role = "change",
    group = "g",
    subgroup = "s",
    time = "t",
    baseline = "b",
    patient = "p",
    covariates = c("a", "b"),
    reference_group = "x",
    reference_subgroup = "x"
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "y ~ b + b:s + b:s:t + b:t + g + g:s + g:s:t + g:t + s + s:t + t",
      "+ a + b + unstr(time = t, gr = p)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + t"
    )
  )
})

test_that("brm_formula() with individual terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = 1,
      TIME = "x",
      BASELINE = 2,
      GROUP = "x",
      USUBJID = "x",
      SUBGROUP = "x"
    ),
    outcome = "CHG",
    role = "change",
    group = "GROUP",
    subgroup = "SUBGROUP",
    time = "TIME",
    baseline = "BASELINE",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x"
  )
  terms <- list(
    baseline = "BASELINE",
    baseline_subgroup = "BASELINE:SUBGROUP",
    baseline_subgroup_time = "BASELINE:SUBGROUP:TIME",
    baseline_time = "BASELINE:TIME",
    group = "GROUP",
    group_subgroup = "GROUP:SUBGROUP",
    group_subgroup_time = "GROUP:SUBGROUP:TIME",
    group_time = "GROUP:TIME",
    subgroup = "SUBGROUP",
    subgroup_time = "SUBGROUP:TIME",
    time = "TIME"
  )
  template <- list(
    data = data,
    baseline = FALSE,
    baseline_subgroup = FALSE,
    baseline_subgroup_time = FALSE,
    baseline_time = FALSE,
    group = FALSE,
    group_subgroup = FALSE,
    group_subgroup_time = FALSE,
    group_time = FALSE,
    subgroup = FALSE,
    subgroup_time = FALSE,
    time = FALSE
  )
  for (intercept in c(TRUE, FALSE)) {
    for (name in names(terms)) {
      args <- template
      args$intercept <- intercept
      args[[name]] <- TRUE
      out <- do.call(what = brm_formula, args = args)
      exp <- "CHG ~"
      if (!intercept) {
        exp <- paste(exp, "0 +")
      }
      exp <- paste(
        exp,
        terms[[name]],
        "+ unstr(time = TIME, gr = USUBJID)"
      )
      expect_equal(deparse(out[[1L]], width.cutoff = 500L), exp)
      expect_equal(
        deparse(out[[2L]][[1L]], width.cutoff = 500L),
        paste(
          "sigma ~ 0 + TIME"
        )
      )
    }
  }
})
