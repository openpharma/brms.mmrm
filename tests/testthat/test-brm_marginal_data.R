test_that("brm_marginal_data()", {
  set.seed(0L)
  sim <- brm_simulate(
    n_group = 2L,
    n_patient = 100L,
    n_time = 4L
  )
  data <- sim$data
  data$group <- paste("treatment", data$group)
  data$time <- paste("visit", data$time)
  data$response[1L] <- NA_real_
  out <- brm_marginal_data(
    data = data,
    response = "response",
    group = "group",
    time = "time",
    level = 0.9
  )
  expect_equal(nrow(out), 56L)
  expect_equal(
    sort(colnames(out)),
    sort(c("statistic", "group", "time", "value"))
  )
  z <- stats::qnorm(p = 0.05)
  for (group in unique(data$group)) {
    for (time in unique(data$time)) {
      response <- data$response[data$group == group & data$time == time]
      mean <- mean(response, na.rm = TRUE)
      median <- median(response, na.rm = TRUE)
      sd <- sd(response, na.rm = TRUE)
      n_observed <- sum(!is.na(response))
      n_total <- length(response)
      lower <- mean - z * sd / sqrt(n_observed)
      upper <- mean + z * sd / sqrt(n_observed)
      exp <- list(
        mean = mean,
        median = median,
        sd = sd,
        n_observed = n_observed,
        n_total = n_total,
        lower = lower,
        upper = upper
      )
      for (field in names(exp)) {
        index <- out$group == group & out$time == time & out$statistic == field
        expect_equal(out$value[index], exp[[field]])
      }
    }
  }
})
