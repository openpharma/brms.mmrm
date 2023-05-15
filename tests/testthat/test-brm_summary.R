test_that("brm_summary()", {
  set.seed(0L)
  sim <- brm_simulate(
    n_group = 2L,
    n_patient = 100L,
    n_time = 4L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  data <- sim$data
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
  out <- brm_summary(
    model = model,
    group = "group",
    time = "time",
    patient = "patient",
    control = 1
  )
  cols <- c(
    "time",
    "group",
    "response_mean",
    "response_lower",
    "response_upper",
    "diff_mean",
    "diff_lower",
    "diff_upper"
  )
  expect_equal(sort(colnames(out)), sort(cols))
  expect_equal(nrow(out), 8L)
  expect_equal(as.integer(out$time), rep(seq_len(4), times = 2))
  expect_equal(as.integer(out$group), rep(seq_len(2), each = 4))
  expect_true(all(out$response_mean > out$response_lower))
  expect_true(all(out$response_mean < out$response_upper))
  expect_true(all(out$diff_mean > out$diff_lower, na.rm = TRUE))
  expect_true(all(out$diff_mean < out$diff_upper, na.rm = TRUE))
  expect_equal(
    out$diff_mean[seq(5, 8)],
    out$response_mean[out$group == 2] - out$response_mean[out$group == 1],
    tolerance = 0.01
  )
})
