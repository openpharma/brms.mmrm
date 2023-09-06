test_that("brm_simulate_lapse()", {
  skip_on_cran()
  set.seed(0L)
  suppressPackageStartupMessages(library(dplyr))
  data <- brm_simulate_start(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L
  ) %>%
    brm_simulate_lapse(rate = 0.57)
  expect_false(any(data$time == 1L & data$missing))
  data <- data[data$time != 1L, ]
  expect_equal(mean(data$missing), 0.57, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$missing), 0.57, tolerance = 0.01)
  }
})
