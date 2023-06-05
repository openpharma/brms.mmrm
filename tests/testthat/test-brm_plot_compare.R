test_that("brm_plot_compare()", {
  set.seed(0L)
  sim <- brm_simulate()
  data <- sim$data
  data$group <- paste("treatment", data$group)
  data$time <- paste("visit", data$time)
  formula <- brm_formula(
    response = "response",
    group = "group",
    time = "time",
    patient = "patient",
    effect_base = FALSE,
    interaction_base = FALSE
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
    group = "group",
    time = "time",
    patient = "patient",
    control = "treatment 1",
    baseline = "visit 1",
    outcome = "response"
  )
  suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
  summaries_data <- brm_marginal_data(
    data,
    response = "response",
    group = "group",
    time = "time"
  )
  out <- brm_plot_compare(
    summaries_draws = summaries_draws,
    summaries_data = summaries_data
  )
  expect_s3_class(out, "ggplot")
})
