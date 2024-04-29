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
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_equal(attr(out, "brm_variance"), "heterogeneous")
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

test_that("brm_formula() same with homogeneous variance", {
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
    time = TRUE,
    variance = "homogeneous"
  )
  expect_s3_class(out, "brmsformula")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_equal(attr(out, "brm_variance"), "homogeneous")
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
    time = TRUE,
    correlation = "autoregressive_moving_average"
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
    residual_covariance_arma_estimation = TRUE
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
    correlation = "autoregressive"
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
    residual_covariance_arma_estimation = TRUE
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
    correlation = "moving_average"
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
    residual_covariance_arma_estimation = TRUE
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
    correlation = "compound_symmetry"
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

test_that("brm_formula() omitting covariates", {
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
    time = TRUE,
    covariates = FALSE
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

test_that("brm_formula_has_subgroup()", {
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
  template <- list(
    data = data,
    intercept = FALSE,
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
  with_subgroup <- c(
    "baseline_subgroup",
    "baseline_subgroup_time",
    "group_subgroup",
    "group_subgroup_time",
    "subgroup",
    "subgroup_time"
  )
  for (term in setdiff(names(template), "data")) {
    args <- template
    args[[term]] <- TRUE
    formula <- do.call(what = brm_formula, args = args)
    expect_equal(
      brm_formula_has_subgroup(formula),
      term %in% with_subgroup
    )
  }
})

test_that("brm_formula() scenario non-subgroup", {
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
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  scenario <- brm_scenario_successive_cells(data)
  out <- brm_formula(scenario)
  expect_s3_class(out, "brms_mmrm_formula_scenario")
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_true(attr(out, "brm_covariates"))
  expect_equal(attr(out, "brm_variance"), "heterogeneous")
  expect_equal(attr(out, "brm_correlation"), "unstructured")
  expect_equal(attr(out, "brm_autoregressive_order"), 1L)
  expect_equal(attr(out, "brm_moving_average_order"), 1L)
  expect_false(attr(out, "brm_residual_covariance_arma_estimation"))
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "change ~ 0 + x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4",
      "+ x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4",
      "+ nuisance_biomarker1 + nuisance_biomarker2 + nuisance_baseline",
      "+ nuisance_status1_absent + nuisance_status1_present",
      "+ nuisance_status2_present",
      "+ unstr(time = time, gr = patient)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + time"
    )
  )
})

test_that("brm_scenario_successive_cells() non-change subgroup w/o covs", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 3,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  scenario <- brm_scenario_successive_cells(data)
  out <- brm_formula(
    scenario,
    covariates = FALSE,
    variance = "homogeneous",
    correlation = "autoregressive",
    autoregressive_order = 2L,
    brm_moving_average_order = 3L,
    residual_covariance_arma_estimation = TRUE
  )
  expect_s3_class(out, "brms_mmrm_formula_scenario")
  expect_s3_class(out, "brms_mmrm_formula")
  expect_s3_class(out, "brmsformula")
  expect_false(attr(out, "brm_covariates"))
  expect_equal(attr(out, "brm_correlation"), "autoregressive")
  expect_equal(attr(out, "brm_variance"), "homogeneous")
  expect_equal(attr(out, "brm_autoregressive_order"), 2L)
  expect_equal(attr(out, "brm_moving_average_order"), 1L)
  expect_true(attr(out, "brm_residual_covariance_arma_estimation"))
  expect_equal(
    trimws(deparse(out[[1L]], width.cutoff = 500L)[1L]),
    paste(
      "response ~ 0 + x_group_1_subgroup_1_time_1 +",
      "x_group_1_subgroup_1_time_2 +",
      "x_group_1_subgroup_1_time_3 + x_group_1_subgroup_2_time_1 +",
      "x_group_1_subgroup_2_time_2 + x_group_1_subgroup_2_time_3 +",
      "x_group_1_subgroup_3_time_1 + x_group_1_subgroup_3_time_2 +",
      "x_group_1_subgroup_3_time_3 + x_group_2_subgroup_1_time_1 +",
      "x_group_2_subgroup_1_time_2 + x_group_2_subgroup_1_time_3 +",
      "x_group_2_subgroup_2_time_1 + x_group_2_subgroup_2_time_2 +",
      "x_group_2_subgroup_2_time_3 + x_group_2_subgroup_3_time_1 +",
      "x_group_2_subgroup_3_time_2 +"
    )
  )
  expect_equal(
    trimws(deparse(out[[1L]], width.cutoff = 500L)[2L]),
    paste(
      "x_group_2_subgroup_3_time_3 +",
      "ar(time = time, gr = patient, p = 2L, cov = TRUE)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 1"
    )
  )
})
