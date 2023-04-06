test_that("brm_simulate() data", {
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  data <- out$data
  expect_equal(dim(data), c(12L, 4L))
  expect_true(is.numeric(data$response))
  expect_false(anyNA(data$response))
  expect_equal(as.integer(data$group), rep(seq_len(2L), each = 6L))
  expect_equal(as.integer(data$patient), rep(seq_len(4L), each = 3L))
  expect_equal(as.integer(data$time), rep(seq_len(3L), times = 4L))
})

test_that("brm_simulate() model_matrix", {
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  matrix <- out$model_matrix
  expect_equal(dim(matrix), c(12L, 4L))
  expect_equal(as.integer(matrix[, "group1"]), rep(c(1L, 0L), each = 6L))
  expect_equal(as.integer(matrix[, "group2"]), rep(c(0L, 1L), each = 6L))
  expect_equal(as.integer(matrix[, "time2"]), as.integer(out$data$time == 2L))
  expect_equal(as.integer(matrix[, "time3"]), as.integer(out$data$time == 3L))
})

test_that("brm_simulate() parameters", {
  set.seed(0L)
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  params <- out$parameters
  expect_equal(names(params), c("beta", "sigma", "covariance"))
  expect_equal(length(params$beta), 4L)
  expect_null(dim(params$beta))
  expect_equal(length(params$sigma), 3L)
  expect_null(dim(params$sigma))
  expect_equal(dim(params$covariance), c(3L, 3L))
  for (value in params) {
    expect_true(all(is.finite(value)))
  }
})
