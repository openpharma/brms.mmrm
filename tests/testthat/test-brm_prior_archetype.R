test_that("brm_prior_archetype() non-subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
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
    )
  archetype <- brm_archetype_successive_cells(data)
  label <- brm_prior_label(
    code = "normal(1, 1)",
    group = "group_1",
    time = "time_1"
  ) |>
    brm_prior_label("normal(1, 2)", group = "group_1", time = "time_2") |>
    brm_prior_label("normal(1, 3)", group = "group_1", time = "time_3") |>
    brm_prior_label("normal(2, 1)", group = "group_2", time = "time_1") |>
    brm_prior_label("normal(2, 2)", group = "group_2", time = "time_2") |>
    brm_prior_label("normal(2, 3)", group = "group_2", time = "time_3")
  prior <- brm_prior_archetype(archetype, label = label)
  expect_s3_class(prior, "brmsprior")
  expect_equal(
    prior$prior,
    c(
      "normal(1, 1)",
      "normal(1, 2)",
      "normal(1, 3)",
      "normal(2, 1)",
      "normal(2, 2)",
      "normal(2, 3)"
    )
  )
  expect_equal(prior$class, rep("b", 6L))
  expect_equal(
    prior$coef,
    c(
      "x_group_1_time_1",
      "x_group_1_time_2",
      "x_group_1_time_3",
      "x_group_2_time_1",
      "x_group_2_time_2",
      "x_group_2_time_3"
    )
  )
})

test_that("brm_prior_archetype() subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 2,
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
    )
  archetype <- brm_archetype_successive_cells(data)
  label <- brm_prior_label(
    code = "normal(1, 1)",
    group = "group_1",
    subgroup = "subgroup_1",
    time = "time_1"
  ) |>
    brm_prior_label(
      "normal(1, 3)",
      group = "group_1",
      subgroup = "subgroup_2",
      time = "time_3"
    ) |>
    brm_prior_label(
      "normal(2, 2)",
      group = "group_2",
      subgroup = "subgroup_1",
      time = "time_2"
    )
  prior <- brm_prior_archetype(archetype, label = label)
  expect_s3_class(prior, "brmsprior")
  expect_equal(
    prior$prior,
    c(
      "normal(1, 1)",
      "normal(1, 3)",
      "normal(2, 2)"
    )
  )
  expect_equal(prior$class, rep("b", 3L))
  expect_equal(
    prior$coef,
    c(
      "x_group_1_subgroup_1_time_1",
      "x_group_1_subgroup_2_time_3",
      "x_group_2_subgroup_1_time_2"
    )
  )
})
