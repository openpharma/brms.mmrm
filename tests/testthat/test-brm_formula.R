test_that("brm_formula() with default names and all non-subgroup terms", {
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
    brm_formula(
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
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    check_rank = FALSE
  )
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_false(attr(out, "brm_model_missing_outcomes"))
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

test_that("same but model missing outcomes", {
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
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    model_missing_outcomes = TRUE,
    check_rank = FALSE
  )
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_true(attr(out, "brm_model_missing_outcomes"))
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG | mi() ~ baseline + baseline:AVISIT +",
      "TRT01P + TRT01P:AVISIT + AVISIT",
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

test_that("brm_formula() same with homogeneous variance", {
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
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    sigma = brm_formula_sigma(
      data,
      check_rank = FALSE,
      time = FALSE,
      intercept = TRUE
    ),
    check_rank = FALSE
  )
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
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
      "sigma ~ 1"
    )
  )
})

test_that("brm_formula() different correlation structures", {
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
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "autoregressive_moving_average",
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ arma(time = AVISIT, gr = USUBJID, p = 1L, q = 1L, cov = FALSE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "autoregressive_moving_average",
    autoregressive_order = 2L,
    moving_average_order = 3L,
    residual_covariance_arma_estimation = TRUE,
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ arma(time = AVISIT, gr = USUBJID, p = 2L, q = 3L, cov = TRUE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "autoregressive",
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ ar(time = AVISIT, gr = USUBJID, p = 1L, cov = FALSE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "autoregressive",
    autoregressive_order = 3L,
    residual_covariance_arma_estimation = TRUE,
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ ar(time = AVISIT, gr = USUBJID, p = 3L, cov = TRUE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "moving_average",
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ ma(time = AVISIT, gr = USUBJID, q = 1L, cov = FALSE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "moving_average",
    moving_average_order = 5L,
    residual_covariance_arma_estimation = TRUE,
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ ma(time = AVISIT, gr = USUBJID, q = 5L, cov = TRUE)"
    )
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    correlation = "compound_symmetry",
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ cosy(time = AVISIT, gr = USUBJID)"
    )
  )
})

test_that("brm_formula() with default names and all terms", {
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
    time = TRUE,
    check_rank = FALSE
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
      y = c(1, 2),
      t = c("x", "y"),
      b = c(2, 3),
      g = c("x", "y"),
      p = c("x", "y"),
      a = c(1, 2)
    ),
    outcome = "y",
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
    time = TRUE,
    check_rank = FALSE
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

test_that("brm_formula() with one user-supplied column, all non-sub terms", {
  data <- brm_data(
    data = tibble::tibble(
      y = c(1, 2),
      t = c("x", "y"),
      b = c(2, 3),
      g = c("x", "y"),
      p = c("x", "y"),
      a = c(1, 2)
    ),
    outcome = "y",
    group = "g",
    time = "t",
    baseline = "b",
    patient = "p",
    covariates = "b",
    reference_group = "x"
  )
  out <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    "y ~ b + b:t + g + g:t + t + b + unstr(time = t, gr = p)"
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + t"
    )
  )
})

test_that("brm_formula() omitting covariates", {
  data <- brm_data(
    data = tibble::tibble(
      y = c(1, 2),
      t = c("x", "y"),
      b = c(2, 3),
      g = c("x", "y"),
      p = c("x", "y"),
      a = c(1, 2)
    ),
    outcome = "y",
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
    time = TRUE,
    covariates = FALSE,
    check_rank = FALSE
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    "y ~ b + b:t + g + g:t + t + unstr(time = t, gr = p)"
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
      y = c(1, 2),
      t = c("x", "y"),
      b = c(2, 3),
      g = c("x", "y"),
      s = c("x", "y"),
      p = c("x", "y"),
      a = c(1, 2)
    ),
    outcome = "y",
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
    time = TRUE,
    check_rank = FALSE
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
      CHG = c(1, 2),
      TIME = c("x", "y"),
      BASELINE = c(2, 3),
      GROUP = c("x", "y"),
      USUBJID = c("x", "y"),
      SUBGROUP = c("x", "y")
    ),
    outcome = "CHG",
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
    time = FALSE,
    check_rank = FALSE
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

test_that("brm_formula() check_rank still works if all outcomes missing", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  )
  out <- brm_formula(data = data, check_rank = TRUE)
  expect_s3_class(out, "brms_mmrm_formula")
})

test_that("brm_formula() archetype non-subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  archetype <- brm_archetype_successive_cells(data)
  expect_warning(
    brm_formula(archetype, baseline = TRUE),
    class = "brm_warn"
  )
  out <- brm_formula(archetype, check_rank = TRUE)
  expect_s3_class(out, "brms_mmrm_formula_archetype")
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_equal(attr(out, "brm_autoregressive_order"), 1L)
  expect_equal(attr(out, "brm_moving_average_order"), 1L)
  expect_false(attr(out, "brm_residual_covariance_arma_estimation"))
  expect_false(attr(out, "brm_model_missing_outcomes"))
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "change ~ 0 + x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4 +",
      "x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4 +",
      "nuisance_biomarker1 + nuisance_biomarker2 + nuisance_status1_absent +",
      "nuisance_status2_present + nuisance_baseline +",
      "nuisance_baseline.timetime_2 + nuisance_baseline.timetime_3 +",
      "unstr(time = time, gr = patient)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + time"
    )
  )
})

test_that("same but model missing outcomes", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  archetype <- brm_archetype_successive_cells(data)
  expect_warning(
    brm_formula(archetype, baseline = TRUE),
    class = "brm_warn"
  )
  out <- brm_formula(
    archetype,
    model_missing_outcomes = TRUE,
    check_rank = TRUE
  )
  expect_s3_class(out, "brms_mmrm_formula_archetype")
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_equal(attr(out, "brm_autoregressive_order"), 1L)
  expect_equal(attr(out, "brm_moving_average_order"), 1L)
  expect_false(attr(out, "brm_residual_covariance_arma_estimation"))
  expect_true(attr(out, "brm_model_missing_outcomes"))
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "change | mi() ~ 0 + x_group_1_time_2 +",
      "x_group_1_time_3 + x_group_1_time_4 +",
      "x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4 +",
      "nuisance_biomarker1 + nuisance_biomarker2 + nuisance_status1_absent +",
      "nuisance_status2_present + nuisance_baseline +",
      "nuisance_baseline.timetime_2 + nuisance_baseline.timetime_3 +",
      "unstr(time = time, gr = patient)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + time"
    )
  )
})
