test_that("brm_marginal_summaries() on response", {
  skip_on_cran()
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
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  suppressWarnings(
    x <- brm_marginal_summaries(
      draws,
      level = 0.95
    )
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("marginal", "group", "time", "statistic", "value", "mcse"))
  )
  tibbles <- c("response", "difference_time", "difference_group", "effect")
  expect_equal(
    sort(unique(x$marginal)),
    sort(tibbles)
  )
  for (marginal in tibbles) {
    groups <- unique(data$group)
    times <- unique(data$time)
    if (marginal %in% c("difference_time", "difference_group")) {
      times <- setdiff(times, "time.1")
    }
    if (identical(marginal, "difference_group")) {
      groups <- setdiff(groups, "difference_group")
    }
    for (group in groups) {
      for (time in times) {
        name <- paste(group, time, sep = brm_sep())
        index <- x$marginal == "response" & x$group == group & x$time == time
        subset <- x[index, ]
        expect_equal(
          unname(subset$value[subset$statistic == "mean"]),
          mean(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "median"]),
          median(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "sd"]),
          sd(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "lower"]),
          unname(quantile(draws$response[[name]], probs = 0.025))
        )
        expect_equal(
          unname(subset$value[subset$statistic == "upper"]),
          unname(quantile(draws$response[[name]], probs = 0.975))
        )
        suppressWarnings({
          expect_equal(
            unname(subset$mcse[subset$statistic == "mean"]),
            posterior::mcse_mean(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "median"]),
            posterior::mcse_median(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "sd"]),
            posterior::mcse_sd(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "lower"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.025
              )
            )
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "upper"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.975
              )
            )
          )
        })
      }
    }
  }
})

test_that("brm_marginal_summaries() on response with subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_subgroup = 2L,
    n_patient = 25L,
    n_time = 4L
  )
  data$response <- rnorm(n = nrow(data))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  suppressWarnings(
    x <- brm_marginal_summaries(
      draws,
      level = 0.95
    )
  )
  expect_equal(
    sort(colnames(x)),
    sort(
      c("marginal", "group", "subgroup", "time", "statistic", "value", "mcse")
    )
  )
  tibbles <- c(
    "response",
    "difference_time",
    "difference_group",
    "difference_subgroup",
    "effect"
  )
  expect_equal(
    sort(unique(x$marginal)),
    sort(tibbles)
  )
  for (marginal in tibbles) {
    groups <- unique(data$group)
    subgroups <- unique(data$subgroup)
    times <- unique(data$time)
    if (any(grepl("^difference_", marginal))) {
      times <- setdiff(times, "time_1")
    }
    if (marginal %in% c("difference_group", "difference_subgroup")) {
      groups <- setdiff(groups, "group_1")
    }
    if (any(marginal == "difference_subgroup")) {
      subgroups <- setdiff(subgroups, "subgroup_1")
    }
    for (group in groups) {
      for (subgroup in subgroups) {
        for (time in times) {
          name <- paste(group, subgroup, time, sep = brm_sep())
          index <- x$marginal == "response" &
            x$group == group &
            x$subgroup == subgroup &
            x$time == time
          subset <- x[index, ]
          expect_equal(
            unname(subset$value[subset$statistic == "mean"]),
            mean(draws$response[[name]])
          )
          expect_equal(
            unname(subset$value[subset$statistic == "median"]),
            median(draws$response[[name]])
          )
          expect_equal(
            unname(subset$value[subset$statistic == "sd"]),
            sd(draws$response[[name]])
          )
          expect_equal(
            unname(subset$value[subset$statistic == "lower"]),
            unname(quantile(draws$response[[name]], probs = 0.025))
          )
          expect_equal(
            unname(subset$value[subset$statistic == "upper"]),
            unname(quantile(draws$response[[name]], probs = 0.975))
          )
          suppressWarnings({
            expect_equal(
              unname(subset$mcse[subset$statistic == "mean"]),
              posterior::mcse_mean(draws$response[[name]])
            )
            expect_equal(
              unname(subset$mcse[subset$statistic == "median"]),
              posterior::mcse_median(draws$response[[name]])
            )
            expect_equal(
              unname(subset$mcse[subset$statistic == "sd"]),
              posterior::mcse_sd(draws$response[[name]])
            )
            expect_equal(
              unname(subset$mcse[subset$statistic == "lower"]),
              unname(
                posterior::mcse_quantile(
                  draws$response[[name]],
                  probs = 0.025
                )
              )
            )
            expect_equal(
              unname(subset$mcse[subset$statistic == "upper"]),
              unname(
                posterior::mcse_quantile(
                  draws$response[[name]],
                  probs = 0.975
                )
              )
            )
          })
        }
      }
    }
  }
})

test_that("brm_marginal_summaries() on change", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate_simple()$data),
    outcome = "response",
    role = "change",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1"
  )
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  suppressWarnings(
    x <- brm_marginal_summaries(
      draws,
      level = 0.9
    )
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("marginal", "group", "time", "statistic", "value", "mcse"))
  )
  expect_equal(
    sort(unique(x$marginal)),
    sort(c("response", "difference_group", "effect"))
  )
  for (marginal in c("response", "difference_group", "effect")) {
    groups <- unique(data$group)
    times <- unique(data$time)
    if (identical(marginal, "difference_group")) {
      groups <- setdiff(groups, "difference_group")
    }
    for (group in groups) {
      for (time in times) {
        name <- paste(group, time, sep = brm_sep())
        index <- x$marginal == "response" & x$group == group & x$time == time
        subset <- x[index, ]
        expect_equal(
          unname(subset$value[subset$statistic == "mean"]),
          mean(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "median"]),
          median(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "sd"]),
          sd(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "lower"]),
          unname(quantile(draws$response[[name]], probs = 0.05))
        )
        expect_equal(
          unname(subset$value[subset$statistic == "upper"]),
          unname(quantile(draws$response[[name]], probs = 0.95))
        )
        suppressWarnings({
          expect_equal(
            unname(subset$mcse[subset$statistic == "mean"]),
            posterior::mcse_mean(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "median"]),
            posterior::mcse_median(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "sd"]),
            posterior::mcse_sd(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "lower"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.05
              )
            )
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "upper"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.95
              )
            )
          )
        })
      }
    }
  }
})
