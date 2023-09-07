test_that("brm_simulate_outline() grid", {
  data <- brm_simulate_outline(
    n_group = 11L,
    n_patient = 12L,
    n_time = 13L,
    rate_dropout = 0.5,
    rate_lapse = 0.5
  )
  expect_equal(nrow(data), 1716L)
  for (field in c("group", "patient", "time")) {
    data[[field]] <- as.integer(gsub(paste0(field, "_"), "", data[[field]]))
  }
  data <- dplyr::arrange(data, group, patient, time)
  expect_equal(
    data$group,
    rep(seq_len(11L), each = 12L * 13L)
  )
  expect_equal(
    data$patient,
    rep(seq_len(11L * 12L), each = 13L)
  )
  expect_equal(
    data$time,
    rep(seq_len(13L), times = 11L * 12L)
  )
})

test_that("brm_simulate_outline() compounded missingness", {
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 10000L,
    n_time = 4L,
    rate_dropout = 0.5,
    rate_lapse = 0.5
  )
  data <- data[data$time != "time_1", ]
  expect_true(mean(data$missing) > 0.65)
})

test_that("brm_simulate_outline() lapsed missing", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L,
    rate_dropout = 0,
    rate_lapse = 0.57
  )
  expect_false(any(data$time == "time_1" & data$missing))
  data <- data[data$time != "time_1", ]
  expect_equal(mean(data$missing), 0.57, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$missing), 0.57, tolerance = 0.01)
  }
})

test_that("brm_simulate_outline() dropout at final time point", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 100000L,
    n_time = 4L,
    rate_dropout = 0.37,
    rate_lapse = 0
  )
  data <- data[data$time == "time_4", ]
  expect_equal(mean(data$missing), 0.37, tolerance = 0.01)
  for (level in unique(data$group)) {
    out <- dplyr::filter(data, group == level)
    expect_equal(mean(out$missing), 0.37, tolerance = 0.01)
  }
})

test_that("brm_simulate_outline() dropouts have correct pattern", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 1000L,
    n_time = 4L,
    rate_dropout = 0.37,
    rate_lapse = 0
  )
  expected <- rbind(
    rep(TRUE, 4L),
    upper.tri(matrix(nrow = 4L, ncol = 4L))
  )
  for (subject in unique(data$patient)) {
    x <- dplyr::filter(data, patient == subject)$missing
    expect_true(any(any(apply(expected, 1, function(row) all(row == x)))))
  }
})

test_that("brm_simulate_outline() correct dropout times", {
  skip_on_cran()
  set.seed(0L)
  suppressPackageStartupMessages(library(dplyr))
  data <- brm_simulate_outline(
    n_group = 2L,
    n_patient = 10000L,
    n_time = 4L,
    rate_dropout = 1,
    rate_lapse = 0
  )
  data <- data[data$time != "time_1", ]
  data <- data %>%
    group_by(patient) %>%
    summarize(drop = min(which(missing)), .groups = "drop")
  expect_lt(diff(range(table(data$drop))), 300L)
})
