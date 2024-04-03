test_that("brm_marginal_probabilities() on response", {
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
  x <- brm_marginal_probabilities(
    draws,
    threshold = 0,
    direction = "greater"
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("group_2", 3))
  expect_equal(x$time, paste0("time_", seq(2, 4)))
  expect_equal(x$direction, rep("greater", 3))
  expect_equal(x$threshold, rep(0, 3))
  column <- function(group, time) {
    sprintf("group_%s%stime_%s", group, brm_sep(), time)
  }
  expect_equal(
    x$value[1L],
    mean(draws$difference_group[[column(2L, 2L)]] > 0)
  )
  expect_equal(
    x$value[2L],
    mean(draws$difference_group[[column(2L, 3L)]] > 0)
  )
  expect_equal(
    x$value[3L],
    mean(draws$difference_group[[column(2L, 4L)]] > 0)
  )
})

test_that("brm_marginal_probabilities() on change and multiple probs", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate_simple()$data),
    outcome = "response",
    role = "change",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1"
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
  for (index in seq_along(draws$difference_group)) {
    draws$difference_group[[index]] <- seq_len(nrow(draws$difference_group))
  }
  x <- brm_marginal_probabilities(
    draws,
    direction = c("less", "greater"),
    threshold = c(15, 30)
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("group_2", 8))
  expect_equal(x$time, rep(paste0("time_", seq(1, 4)), times = 2))
  expect_equal(x$direction, rep(c("greater", "less"), each = 4))
  expect_equal(x$threshold, c(rep(30, 4), rep(15, 4)))
  expect_equal(x$value, rep(c(0.4, 0.28), each = 4L))
})

test_that("brm_marginal_probabilities() with subgroup", {
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
  for (index in seq_along(draws$difference_group)) {
    draws$difference_group[[index]] <- seq_len(nrow(draws$difference_group))
  }
  x <- brm_marginal_probabilities(
    draws,
    direction = c("less", "greater"),
    threshold = c(15, 30)
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "subgroup", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("group_2", 12))
  expect_equal(
    x$subgroup,
    rep(rep(c("subgroup_1", "subgroup_2"), each = 3), times = 2)
  )
  expect_equal(x$time, rep(paste0("time_", seq(2, 4)), times = 4))
  expect_equal(x$direction, rep(c("greater", "less"), each = 6))
  expect_equal(x$threshold, c(rep(30, 6), rep(15, 6)))
  expect_equal(x$value, rep(c(0.4, 0.28), each = 6))
})
