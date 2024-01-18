test_that("brm_marginal_data()", {
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate_simple()$data),
    outcome = "response",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  data$response[1L] <- NA_real_
  out <- brm_marginal_data(
    data = data,
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

test_that("brm_marginal_data() subgroup", {
  set.seed(0L)
  data <- brm_data(
    data = tidyr::expand_grid(
      tibble::as_tibble(brm_simulate_simple()$data),
      tibble::tibble(subgroup = c("subgroup_1", "subgroup_2"))
    ),
    outcome = "response",
    role = "response",
    group = "group",
    subgroup = "subgroup",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_subgroup = "subgroup_1",
    reference_time = "time_1"
  )
  data$response <- rnorm(n = nrow(data))
  data$response[1L] <- NA_real_
  out <- brm_marginal_data(
    data = data,
    level = 0.9,
    subgroup = TRUE
  )
  expect_equal(nrow(out), 112L)
  expect_equal(
    sort(colnames(out)),
    sort(c("statistic", "group", "subgroup", "time", "value"))
  )
  z <- stats::qnorm(p = 0.05)
  for (group in unique(data$group)) {
    for (subgroup in unique(data$subgroup)) {
      for (time in unique(data$time)) {
        filter <- data$group == group &
          data$subgroup == subgroup &
          data$time == time
        response <- data$response[filter]
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
          filter <- out$group == group &
            out$subgroup == subgroup &
            out$time == time &
            out$statistic == field
          expect_equal(out$value[filter], exp[[field]])
        }
      }
    }
  }
})
