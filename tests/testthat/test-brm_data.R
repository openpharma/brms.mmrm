test_that("brm_data() good", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  out <- brm_data(
    data = data,
    outcome = "col_response",
    role = "response",
    group = "col_group",
    time = "col_time",
    patient = "col_patient",
    covariates = c("col_factor2", "col_factor3")
  )
  expect_s3_class(out, "brm_data")
  expect_true(tibble::is_tibble(out))
  expect_silent(brm_data_validate(out))
  expect_true(all(is.na(out$col_response[c(2L, 3L)])))
  expect_false(anyNA(out$col_response[- c(2L, 3L)]))
  expect_equal(nrow(out), 800L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "col_patient",
        "col_time",
        "col_response",
        "col_group",
        "col_factor2",
        "col_factor3"
      )
    )
  )
  expect_equal(
    out$col_group,
    as.factor(rep(c(1L, 2L), each = 400L))
  )
  expect_equal(
    out$col_time,
    as.factor(rep(seq_len(4L), times = 200L))
  )
  expect_equal(
    sort(out$col_response[- c(2L, 3L)]),
    sort(c(data$col_response))
  )
  expect_equal(out$col_patient, out$col_factor2)
  expect_equal(out$col_patient, out$col_factor3)
})

test_that("brm_data() bad role", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "nope",
      role = "response",
      group = "col_group",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})

test_that("brm_data() bad group", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "nope",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})
