test_that("brm_marginal_draws() on response", {
  set.seed(0L)
  sim <- brm_simulate(
    n_group = 2L,
    n_patient = 100L,
    n_time = 4L
  )
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
  out <- brm_marginal_draws(
    model = model,
    group = "group",
    time = "time",
    patient = "patient",
    control = "treatment 1",
    baseline = "visit 1",
    outcome = "response"
  )
  fields <- c("response", "change", "difference")
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(columns_df$group, columns_df$time, sep = ", ")
  expect_equal(sort(names(out)), sort(fields))
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$time != "visit 1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = ", ")
  expect_equal(
    sort(colnames(out$change)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "treatment 1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = ", ")
  expect_equal(
    sort(colnames(out$difference)),
    sort(c(columns, names_mcmc))
  )
  for (group in setdiff(unique(data$group), "treatment 1")) {
    for (time in setdiff(unique(data$time), "visit 1")) {
      name1 <- paste("treatment 1", time, sep = ", ")
      name2 <- paste(group, time, sep = ", ")
      expect_equal(
        out$difference[[name2]],
        out$change[[name2]] - out$change[[name1]]
      )
    }
  }
  for (group in unique(data$group)) {
    for (time in setdiff(unique(data$time), "visit 1")) {
      name1 <- paste(group, "visit 1", sep = ", ")
      name2 <- paste(group, time, sep = ", ")
      expect_equal(
        out$change[[name2]],
        out$response[[name2]] - out$response[[name1]]
      )
    }
  }
})

test_that("brm_marginal_draws() on change", {
  set.seed(0L)
  sim <- brm_simulate(
    n_group = 2L,
    n_patient = 100L,
    n_time = 4L
  )
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
  out <- brm_marginal_draws(
    model = model,
    group = "group",
    time = "time",
    patient = "patient",
    control = "treatment 1",
    baseline = "visit 1",
    outcome = "change"
  )
  fields <- c("response", "difference")
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(columns_df$group, columns_df$time, sep = ", ")
  expect_equal(sort(names(out)), sort(fields))
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "treatment 1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = ", ")
  expect_equal(
    sort(colnames(out$difference)),
    sort(c(columns, names_mcmc))
  )
  for (group in setdiff(unique(data$group), "treatment 1")) {
    for (time in unique(data$time)) {
      name1 <- paste("treatment 1", time, sep = ", ")
      name2 <- paste(group, time, sep = ", ")
      expect_equal(
        out$difference[[name2]],
        out$response[[name2]] - out$response[[name1]]
      )
    }
  }
})
