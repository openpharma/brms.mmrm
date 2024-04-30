test_that("unname_df()", {
  x <- unname_df(tibble::tibble(x = c(a = 1, b = 2), y = c(c = 3, d = 4)))
  expect_null(names(x$x))
  expect_null(names(x$y))
})

test_that("zero_pad_integers", {
  expect_equal(
    zero_pad_integers(c(1L, 0L, 2L, 5L, 7L)),
    c("1", "0", "2", "5", "7")
  )
  expect_equal(
    zero_pad_integers(c(1L, 10L, 0L, 2L, 5L, 7L)),
    c("01", "10", "00", "02", "05", "07")
  )
  expect_equal(
    zero_pad_integers(c(1L, 10L, 0L, 207L, 2L, 5L, 7L)),
    c("001", "010", "000", "207", "002", "005", "007")
  )
})

test_that("brm_has_subgroup() on regular data", {
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
    time = FALSE,
    check_rank = FALSE
  )
  with_subgroup <- c(
    "baseline_subgroup",
    "baseline_subgroup_time",
    "group_subgroup",
    "group_subgroup_time",
    "subgroup",
    "subgroup_time"
  )
  for (term in setdiff(names(template), c("data", "check_rank"))) {
    args <- template
    args[[term]] <- TRUE
    formula <- do.call(what = brm_formula, args = args)
    expect_equal(
      brm_has_subgroup(data = data, formula = formula),
      term %in% with_subgroup
    )
  }
})

test_that("brm_has_subgroup() on scenario", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
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
  scenario <- brm_scenario_successive_cells(data)
  formula <- brm_formula(scenario)
  expect_false(brm_has_subgroup(scenario, formula))
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 3,
    n_patient = 100,
    n_time = 3,
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
  scenario <- brm_scenario_successive_cells(data)
  formula <- brm_formula(scenario)
  expect_true(brm_has_subgroup(scenario, formula))
})

test_that("brm_has_nuisance() on regular data", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      TIME = c("x", "y"),
      BASELINE = c(2, 3),
      GROUP = c("x", "y"),
      USUBJID = c("x", "y"),
      SUBGROUP = c("x", "y"),
      FACTOR = c("x", "y")
    ),
    outcome = "CHG",
    role = "change",
    group = "GROUP",
    subgroup = "SUBGROUP",
    time = "TIME",
    baseline = "BASELINE",
    patient = "USUBJID",
    reference_group = "x",
    reference_subgroup = "x",
    covariates = "FACTOR"
  )
  template <- list(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    baseline_subgroup = FALSE,
    baseline_subgroup_time = FALSE,
    baseline_time = FALSE,
    covariates = FALSE,
    group = FALSE,
    group_subgroup = FALSE,
    group_subgroup_time = FALSE,
    group_time = FALSE,
    subgroup = FALSE,
    subgroup_time = FALSE,
    time = FALSE,
    check_rank = FALSE
  )
  with_nuisance <- c(
    "baseline",
    "baseline_subgroup",
    "baseline_subgroup_time",
    "baseline_time",
    "covariates"
  )
  for (term in setdiff(names(template), c("data", "check_rank"))) {
    args <- template
    args[[term]] <- TRUE
    formula <- do.call(what = brm_formula, args = args)
    expect_equal(
      brm_has_nuisance(data = data, formula = formula),
      term %in% with_nuisance
    )
  }
})

test_that("brm_has_nuisance() on scenario", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  scenario <- brm_scenario_successive_cells(data)
  formula <- brm_formula(scenario)
  expect_false(brm_has_nuisance(scenario, formula))
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
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
  scenario <- brm_scenario_successive_cells(data)
  formula <- brm_formula(scenario)
  expect_true(brm_has_nuisance(scenario, formula))
})
