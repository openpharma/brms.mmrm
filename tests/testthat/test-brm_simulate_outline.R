test_that("brm_simulate_outline() grid", {
  data <- brm_simulate_outline(
    n_group = 11L,
    n_patient = 12L,
    n_time = 13L,
    baseline = FALSE,
    rate_dropout = 0.1,
    rate_lapse = 0.025
  )
  expect_equal(dim(data), c(1716L, 5L))
  expect_equal(data$group, paste("group", rep(seq_len(11L), each = 12L * 13L)))
  expect_equal(
    data$patient,
    paste("patient", rep(seq_len(11L * 12L), each = 13L))
  )
  expect_equal(data$time, paste("time", rep(seq_len(13L), times = 11L * 12L)))
})

test_that("brm_simulate_outline() miss_lapse without baseline", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L,
    baseline = FALSE,
    rate_dropout = 0.1,
    rate_lapse = 0.57
  )
  expect_equal(mean(data$miss_lapse), 0.57, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$miss_lapse), 0.57, tolerance = 0.01)
  }
  expect_true(any(data$time == "time 1" & data$miss_lapse))
})

test_that("brm_simulate_outline() miss_lapse with baseline", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L,
    baseline = TRUE,
    rate_dropout = 0.1,
    rate_lapse = 0.57
  )
  expect_false(any(data$time == "time 1" & data$miss_lapse))
  data <- data[data$time != "time 1", ]
  expect_equal(mean(data$miss_lapse), 0.57, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$miss_lapse), 0.57, tolerance = 0.01)
  }
})

test_that("brm_simulate_outline() miss_dropout at final time point", {
  skip_on_cran()
  for (baseline in c(TRUE, FALSE)) {
    set.seed(0L)
    data <- brm_simulate_outline(
      n_group = 2L,
      n_patient = 100000L,
      n_time = 4L,
      baseline = baseline,
      rate_dropout = 0.37,
      rate_lapse = 0.57
    )
    data <- data[data$time == "time 4", ]
    expect_equal(mean(data$miss_dropout), 0.37, tolerance = 0.01)
    for (level in unique(data$group)) {
      out <- dplyr::filter(data, group == level)
      expect_equal(mean(out$miss_dropout), 0.37, tolerance = 0.01)
    }
  }
})

test_that("brm_simulate_outline() dropouts have correct pattern", {
  skip_on_cran()
  for (baseline in c(TRUE, FALSE)) {
    set.seed(0L)
    data <- brm_simulate_outline(
      n_group = 2L,
      n_patient = 1000L,
      n_time = 4L,
      baseline = baseline,
      rate_dropout = 0.37,
      rate_lapse = 0.57
    )
    expected <- rbind(
      rep(TRUE, 4L),
      upper.tri(matrix(nrow = 4L, ncol = 4L))
    )
    for (subject in unique(data$patient)) {
      x <- dplyr::filter(data, patient == subject)$miss_dropout
      expect_true(any(any(apply(expected, 1, function(row) all(row == x)))))
    }
  }
})

test_that("brm_simulate_outline() correct dropout times", {
  skip_on_cran()
  for (baseline in c(TRUE, FALSE)) {
    set.seed(0L)
    suppressPackageStartupMessages(library(dplyr))
    data <- brm_simulate_outline(
      n_group = 2L,
      n_patient = 10000L,
      n_time = 4L,
      baseline = baseline,
      rate_dropout = 1,
      rate_lapse = 0.57
    )
    if (baseline) {
      data <- data[data$time != "time 1", ]
    }
    data <- data %>%
      group_by(patient) %>%
      summarize(drop = min(which(miss_dropout)), .groups = "drop")
    expect_lt(diff(range(table(data$drop))), 300L)
  }
})
