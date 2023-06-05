test_that("brm_marginal_probabilities() on response", {
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
  x <- brm_marginal_probabilities(
    draws,
    threshold = 0,
    direction = "greater"
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("treatment 2", 3))
  expect_equal(x$time, paste("visit", seq(2, 4)))
  expect_equal(x$direction, rep("greater", 3))
  expect_equal(x$threshold, rep(0, 3))
  expect_equal(
    x$value[1L],
    mean(draws$difference[["treatment 2, visit 2"]] > 0)
  )
  expect_equal(
    x$value[2L],
    mean(draws$difference[["treatment 2, visit 3"]] > 0)
  )
  expect_equal(
    x$value[3L],
    mean(draws$difference[["treatment 2, visit 4"]] > 0)
  )
})

test_that("brm_marginal_probabilities() on change and multiple probs", {
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
    outcome = "change"
  )
  x <- brm_marginal_probabilities(
    draws,
    direction = c("less", "greater"),
    threshold = c(-1.55, -1.7)
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("treatment 2", 8))
  expect_equal(x$time, rep(paste("visit", seq(1, 4)), times = 2))
  expect_equal(x$direction, rep(c("greater", "less"), each = 4))
  expect_equal(x$threshold, c(rep(-1.7, 4), rep(-1.55, 4)))
  expect_equal(
    x$value[1L],
    mean(draws$difference[["treatment 2, visit 1"]] > -1.7)
  )
  expect_equal(
    x$value[2L],
    mean(draws$difference[["treatment 2, visit 2"]] > -1.7)
  )
  expect_equal(
    x$value[3L],
    mean(draws$difference[["treatment 2, visit 3"]] > -1.7)
  )
  expect_equal(
    x$value[4L],
    mean(draws$difference[["treatment 2, visit 4"]] > -1.7)
  )
  expect_equal(
    x$value[5L],
    mean(draws$difference[["treatment 2, visit 1"]] < -1.55)
  )
  expect_equal(
    x$value[6L],
    mean(draws$difference[["treatment 2, visit 2"]] < -1.55)
  )
  expect_equal(
    x$value[7L],
    mean(draws$difference[["treatment 2, visit 3"]] < -1.55)
  )
  expect_equal(
    x$value[8L],
    mean(draws$difference[["treatment 2, visit 4"]] < -1.55)
  )
})
