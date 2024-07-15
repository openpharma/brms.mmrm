test_that("brm_model() runs, assume MAR", {
  skip_on_cran()
  set.seed(0L)
  data <- tibble::as_tibble(brm_simulate_simple()$data)
  data$response[1L] <- NA_real_
  data <- brm_data(
    data = data,
    outcome = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  formula <- brm_formula(
    data = data,
    model_missing_outcomes = FALSE,
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
  expect_s3_class(model, "brmsfit")
  expect_equal(nrow(model$data), 799L)
  draws <- posterior::as_draws_df(model)
  expect_false("ymi[1]" %in% tolower(colnames(draws)))
})

test_that("brm_model() runs, impute missing values during model", {
  skip_on_cran()
  set.seed(0L)
  data <- tibble::as_tibble(brm_simulate_simple()$data)
  data$response[1L] <- NA_real_
  data <- brm_data(
    data = data,
    outcome = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  formula <- brm_formula(
    data = data,
    model_missing_outcomes = TRUE,
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
  expect_s3_class(model, "brmsfit")
  expect_equal(nrow(model$data), 800L)
  draws <- posterior::as_draws_df(model)
  expect_true("ymi[1]" %in% tolower(colnames(draws)))
})
