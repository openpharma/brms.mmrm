test_that("brm_simulate_dropout() at final time point", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_start(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L
  ) %>%
    brm_simulate_dropout(rate = 0.37)
  data <- data[data$time == 4L, ]
  expect_equal(mean(data$missing), 0.37, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$missing), 0.37, tolerance = 0.01)
  }
})

test_that("brm_simulate_dropout() dropouts have correct pattern", {
  skip_on_cran()
  suppressPackageStartupMessages(library(dplyr))
  set.seed(0L)
  data <- brm_simulate_start(
    n_group = 2L,
    n_patient = 1000L,
    n_time = 4L
  ) %>%
    brm_simulate_dropout(rate = 0.57)
  expected <- upper.tri(matrix(nrow = 4L, ncol = 4L))
  data %>%
    group_by(patient) %>%
    group_walk(
      ~expect_true(
        any(apply(expected, 1, function(row) all(row == .x$missing)))
      )
    )
})

test_that("brm_simulate_dropout() correct dropout times", {
  skip_on_cran()
  set.seed(0L)
  suppressPackageStartupMessages(library(dplyr))
  data <- brm_simulate_start(
    n_group = 2L,
    n_patient = 10000L,
    n_time = 4L
  ) %>%
    brm_simulate_dropout(rate = 1)
  data <- data[data$time > min(data$time), ]
  data <- data %>%
    group_by(patient) %>%
    summarize(drop = min(which(missing)), .groups = "drop")
  expect_lt(diff(range(table(data$drop))), 300L)
})
