test_that("brm_simulate_start() creates the right grid", {
  data <- brm_simulate_start(n_group = 11L, n_patient = 12L, n_time = 13L)
  expect_equal(dim(data), c(1716L, 3L))
  expect_equal(data$group, rep(seq_len(11L), each = 12L * 13L))
  expect_equal(data$patient, rep(seq_len(11L * 12L), each = 13L))
  expect_equal(data$time, rep(seq_len(13L), times = 11L * 12L))
})
