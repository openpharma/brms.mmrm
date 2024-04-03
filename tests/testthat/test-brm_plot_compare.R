test_that("brm_plot_compare() without subgroup", {
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
    formula = formula,
    data = data
  )
  suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
  summaries_data <- brm_marginal_data(data)
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "time",
      facet = "group"
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "group",
      facet = "time"
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "group",
      axis = "time",
      facet = "source"
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "time",
      axis = "source",
      facet = "group"
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "time",
      axis = "group",
      facet = "source"
    ),
    "ggplot"
  )
})

test_that("brm_plot_compare() with subgroups", {
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
    formula = formula,
    data = data
  )
  suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
  summaries_data <- brm_marginal_data(data)
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "time",
      facet = c("group", "subgroup")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "subgroup",
      axis = "time",
      facet = c("source", "group")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "time",
      facet = c("subgroup", "group")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "group",
      facet = c("time", "subgroup")
    ),
    "ggplot"
  )
  expect_s3_class(
    brm_plot_compare(
      summaries_draws = summaries_draws,
      summaries_data = summaries_data,
      compare = "source",
      axis = "subgroup",
      facet = c("time", "group")
    ),
    "ggplot"
  )
})
