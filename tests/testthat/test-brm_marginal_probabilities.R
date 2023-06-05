test_that("brm_summary() on response", {
  set.seed(0L)
  sim <- brm_simulate()
  data <- sim$data
  data$group <- paste("treatment", data$group)
  data$time <- paste("visit", data$time)
  formula <- brm_formula(
    response = "response",
    group = "group",
    time = "time",
    patient = "patient",
    effect_base = FALSE,
    interaction_base = FALSE
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
  marginals <- brm_marginal_draws(
    model = model,
    group = "group",
    time = "time",
    patient = "patient",
    control = "treatment 1",
    baseline = "visit 1",
    outcome = "response"
  )
  suppressWarnings(
    out <- brm_summary(
      marginals,
      level = 0.95,
      threshold = 0,
      direction = "greater"
    )
  )
  expect_equal(sort(names(out)), sort(c("means", "probabilities")))
  for (field in names(out)) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_false(any(unlist(lapply(x, anyNA))))
  }
  x <- out$means
  expect_equal(
    sort(colnames(x)),
    sort(c("marginal", "group", "time", "statistic", "value", "mcse"))
  )
  for (marginal in c("response", "change", "difference")) {
    groups <- unique(data$group)
    times <- unique(data$time)
    if (marginal %in% c("change", "difference")) {
      times <- setdiff(times, "visit 1")
    }
    if (identical(marginal, "difference")) {
      groups <- setdiff(groups, "difference")
    }
    for (group in groups) {
      for (time in times) {
        name <- paste(group, time, sep = ", ")
        index <- x$marginal == "response" & x$group == group & x$time == time
        subset <- x[index, ]
        expect_equal(
          unname(subset$value[subset$statistic == "mean"]),
          mean(marginals$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "median"]),
          median(marginals$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "sd"]),
          sd(marginals$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "lower"]),
          unname(quantile(marginals$response[[name]], probs = 0.025))
        )
        expect_equal(
          unname(subset$value[subset$statistic == "upper"]),
          unname(quantile(marginals$response[[name]], probs = 0.975))
        )
        suppressWarnings({
          expect_equal(
            unname(subset$mcse[subset$statistic == "mean"]),
            posterior::mcse_mean(marginals$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "median"]),
            posterior::mcse_median(marginals$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "sd"]),
            posterior::mcse_sd(marginals$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "lower"]),
            unname(
              posterior::mcse_quantile(
                marginals$response[[name]],
                probs = 0.025
              )
            )
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "upper"]),
            unname(
              posterior::mcse_quantile(
                marginals$response[[name]],
                probs = 0.975
              )
            )
          )
        })
      }
    }
  }
  x <- out$probabilities
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("treatment 2", 3))
  expect_equal(x$time, paste("visit", seq(2, 4)))
  expect_equal(x$direction, rep("greater", 3))
  expect_equal(x$threshold, rep(0, 3))
  expect_equal(
    x$value[1L],
    mean(marginals$difference[["treatment 2, visit 2"]] > 0)
  )
  expect_equal(
    x$value[2L],
    mean(marginals$difference[["treatment 2, visit 3"]] > 0)
  )
  expect_equal(
    x$value[3L],
    mean(marginals$difference[["treatment 2, visit 4"]] > 0)
  )
})
