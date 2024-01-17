test_that("brm_simulate_prior() runs", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline()
  data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        out <- brm_simulate_prior(
          data = data,
          formula = formula
        )
      )
    )
  )
  expect_equal(
    sort(names(out)),
    sort(c("data", "model", "model_matrix", "outcome", "parameters"))
  )
  out_data_part <- out$data
  out_data_part$response <- NULL
  data$response <- NULL
  expect_equal(tibble::as_tibble(out_data_part), tibble::as_tibble(data))
  expect_true(any(out$data$missing))
  expect_true(!all(out$data$missing))
  expect_equal(out$data$missing, is.na(out$data$response))
  expect_s3_class(out$model, "brmsfit")
  expect_true(is.matrix(out$model_matrix))
  expect_equal(nrow(out$model_matrix), nrow(out$data))
  expect_true(is.matrix(out$outcome))
  expect_equal(ncol(out$outcome), nrow(out$data))
  expect_true(is.data.frame(out$parameters))
})
