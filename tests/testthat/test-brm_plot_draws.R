test_that("brm_plot_draws() without subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate_simple()$data),
    outcome = "response",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    data = data
  )
  expect_s3_class(
    brm_plot_draws(draws = draws$difference_time),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "time",
      facet = "group"
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "group",
      facet = "time"
    ),
    "ggplot"
  )
})

test_that("brm_plot_draws() with subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_subgroup = 2L,
    n_patient = 25L,
    n_time = 4L
  )
  data$response <- rnorm(n = nrow(data))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    data = data
  )
  expect_s3_class(
    brm_plot_draws(draws = draws$difference_time),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "time",
      facet = c("group", "subgroup")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "time",
      facet = c("subgroup", "group")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "group",
      facet = c("time", "subgroup")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_draws(
      draws = draws$difference_time,
      versus = "subgroup",
      facet = c("time", "group")
    ),
    "ggplot"
  )
})
