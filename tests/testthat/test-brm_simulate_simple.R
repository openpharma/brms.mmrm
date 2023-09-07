test_that("brm_simulate_simple() data", {
  set.seed(0L)
  out <- brm_simulate_simple(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_tau = 0.1,
    hyper_lambda = 1
  )
  data <- out$data
  expect_equal(dim(data), c(12L, 4L))
  expect_true(is.numeric(data$response))
  expect_false(anyNA(data$response))
  expect_equal(data$group, paste0("group_", rep(seq_len(2L), each = 6L)))
  expect_equal(data$patient, paste0("patient_", rep(seq_len(4L), each = 3L)))
  levels_time <- paste0("time_", seq_len(3L))
  expect_equal(data$time, rep(levels_time, times = 4L))
})

test_that("brm_simulate_simple() model_matrix", {
  set.seed(0L)
  out <- brm_simulate_simple(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_tau = 0.1,
    hyper_lambda = 1
  )
  matrix <- out$model_matrix
  expect_equal(dim(matrix), c(12L, 4L))
  colnames(matrix) <- gsub("^group", "", colnames(matrix))
  colnames(matrix) <- gsub("^time", "", colnames(matrix))
  expect_equal(as.integer(matrix[, "group_1"]), rep(c(1L, 0L), each = 6L))
  expect_equal(as.integer(matrix[, "group_2"]), rep(c(0L, 1L), each = 6L))
  expect_equal(
    as.integer(matrix[, "time_2"]),
    as.integer(out$data$time == "time_2")
  )
  expect_equal(
    as.integer(matrix[, "time_3"]),
    as.integer(out$data$time == "time_3")
  )
})

test_that("brm_simulate_simple() parameters", {
  set.seed(0L)
  set.seed(0L)
  out <- brm_simulate_simple(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_tau = 0.1,
    hyper_lambda = 1
  )
  params <- out$parameters
  expect_equal(
    sort(names(params)),
    sort(c("beta", "sigma", "tau", "lambda", "covariance"))
  )
  expect_equal(length(params$beta), 4L)
  expect_null(dim(params$beta))
  expect_equal(length(params$sigma), 3L)
  expect_equal(length(params$tau), 3L)
  expect_null(dim(params$sigma))
  expect_equal(params$tau, log(params$sigma))
  expect_equal(dim(params$lambda), c(3L, 3L))
  expect_equal(dim(params$covariance), c(3L, 3L))
  expect_equal(
    diag(params$sigma) %*% params$lambda %*% diag(params$sigma),
    params$covariance
  )
  for (value in params) {
    expect_true(all(is.finite(value)))
  }
})
