test_that("brm_scenario_successive_cells() change and non-subgroup", {
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
  out <- brm_scenario_successive_cells(data)
  out2 <- brm_scenario_successive_cells(out)
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_successive_cells")
  expect_s3_class(out, "brms_mmrm_scenario")
  attributes_data <- brm_data_attributes(data)
  attributes_scenario <- brm_data_attributes(out)
  attributes_scenario$brm_scenario_parameterization <- NULL
  attributes_scenario$brm_scenario_interest <- NULL
  attributes_scenario$brm_scenario_nuisance <- NULL
  expect_equal(attributes_data, attributes_scenario)
  interest <- attr(out, "brm_scenario_interest")
  nuisance <- attr(out, "brm_scenario_nuisance")
  expect_equal(
    sort(interest),
    sort(
      c(
        "x_group_1_time_2",
        "x_group_1_time_3",
        "x_group_1_time_4",
        "x_group_2_time_2",
        "x_group_2_time_3",
        "x_group_2_time_4"
      )
    )
  )
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "nuisance_biomarker1",
        "nuisance_biomarker2",
        "nuisance_baseline",
        "nuisance_status1absent",
        "nuisance_status1present",
        "nuisance_status2present"
      )
    )
  )
  param <- attr(out, "brm_scenario_parameterization")
  expect_equal(param$variable, interest)
  expect_equal(param$group, rep(c("group_1", "group_2"), each = 3L))
  expect_equal(param$time, rep(c("time_2", "time_3", "time_4"), times = 2L))
})

test_that("brm_scenario_successive_cells() non-change subgroup", {
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
  out <- brm_scenario_successive_cells(data)
  out2 <- brm_scenario_successive_cells(out)
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_successive_cells")
  expect_s3_class(out, "brms_mmrm_scenario")
  attributes_data <- brm_data_attributes(data)
  attributes_scenario <- brm_data_attributes(out)
  attributes_scenario$brm_scenario_parameterization <- NULL
  attributes_scenario$brm_scenario_interest <- NULL
  attributes_scenario$brm_scenario_nuisance <- NULL
  expect_equal(attributes_data, attributes_scenario)
  interest <- attr(out, "brm_scenario_interest")
  nuisance <- attr(out, "brm_scenario_nuisance")
  expect_equal(
    sort(interest),
    sort(
      c(
        "x_group_1_subgroup_1_time_1",
        "x_group_1_subgroup_1_time_2",
        "x_group_1_subgroup_1_time_3",
        "x_group_1_subgroup_2_time_1",
        "x_group_1_subgroup_2_time_2",
        "x_group_1_subgroup_2_time_3",
        "x_group_1_subgroup_3_time_1",
        "x_group_1_subgroup_3_time_2",
        "x_group_1_subgroup_3_time_3",
        "x_group_2_subgroup_1_time_1",
        "x_group_2_subgroup_1_time_2",
        "x_group_2_subgroup_1_time_3",
        "x_group_2_subgroup_2_time_1",
        "x_group_2_subgroup_2_time_2",
        "x_group_2_subgroup_2_time_3",
        "x_group_2_subgroup_3_time_1",
        "x_group_2_subgroup_3_time_2",
        "x_group_2_subgroup_3_time_3"
      )
    )
  )
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "nuisance_biomarker1",
        "nuisance_biomarker2",
        "nuisance_status1absent",
        "nuisance_status1present",
        "nuisance_status2present"
      )
    )
  )
  param <- attr(out, "brm_scenario_parameterization")
  expect_equal(param$variable, interest)
  expect_equal(param$group, rep(c("group_1", "group_2"), each = 9L))
  expect_equal(
    param$subgroup,
    rep(
      rep(c("subgroup_1", "subgroup_2", "subgroup_3"), each = 3L),
      times = 2L
    )
  )
  expect_equal(param$time, rep(c("time_1", "time_2", "time_3"), times = 6L))
})
