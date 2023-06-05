test_that("brm_model() runs", {
  set.seed(0L)
  data <- brm_simulate(
    n_group = 2L,
    n_patient = 100L,
    n_time = 4L
  )$data
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
  expect_s3_class(model, "brmsfit")
})
