test_that("brm_marginal_draws_average() non-subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = brm_simulate_simple()$data,
    outcome = "response",
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
  suppressMessages(
    out <- brm_marginal_draws(
      model = model,
      formula = formula,
      data = data
    )
  )
  averages_all <- brm_marginal_draws_average(
    draws = out,
    data = data,
    label = "average"
  )
  averages_sub <- brm_marginal_draws_average(
    draws = out,
    data = data,
    times = c("time_2", "time_3"),
    label = "mean"
  )
  for (group in unique(data$group)) {
    cols_all <- grep(
      pattern = group,
      x = colnames(out$response),
      fixed = TRUE,
      value = TRUE
    )
    cols_sub <- grep(
      pattern = "time_2|time_3",
      x = cols_all,
      fixed = FALSE,
      value = TRUE
    )
    expect_equal(
      averages_all$response[[name_marginal(group, "average")]],
      apply(tibble::as_tibble(out$response)[, cols_all], 1, mean)
    )
    expect_equal(
      averages_sub$response[[name_marginal(group, "mean")]],
      apply(tibble::as_tibble(out$response)[, cols_sub], 1, mean)
    )
    cols_all <- intersect(cols_all, colnames(out$difference_time))
    cols_sub <- intersect(cols_sub, colnames(out$difference_time))
    expect_equal(
      averages_all$difference_time[[name_marginal(group, "average")]],
      apply(tibble::as_tibble(out$difference_time)[, cols_all], 1, mean)
    )
    expect_equal(
      averages_sub$difference_time[[name_marginal(group, "mean")]],
      apply(tibble::as_tibble(out$difference_time)[, cols_sub], 1, mean)
    )
    cols_all <- intersect(cols_all, colnames(out$difference_group))
    cols_sub <- intersect(cols_sub, colnames(out$difference_group))
    if (group != "group_1") {
      for (field in c("difference_group", "effect")) {
        expect_equal(
          averages_all[[field]][[name_marginal(group, "average")]],
          apply(tibble::as_tibble(out[[field]])[, cols_all], 1, mean)
        )
        expect_equal(
          averages_sub[[field]][[name_marginal(group, "mean")]],
          apply(tibble::as_tibble(out[[field]])[, cols_sub], 1, mean)
        )
      }
    }
  }
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_all))))
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_sub))))
  expect_true(is.list(brm_marginal_probabilities(averages_all)))
  expect_true(is.list(brm_marginal_probabilities(averages_sub)))
})

test_that("brm_marginal_draws_average() subgroup", {
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
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  averages_all <- brm_marginal_draws_average(
    draws = out,
    data = data,
    label = "average"
  )
  averages_sub <- brm_marginal_draws_average(
    draws = out,
    data = data,
    times = c("time_2", "time_3"),
    label = "mean"
  )
  for (group in unique(data$group)) {
    for (subgroup in unique(data$subgroup)) {
      cols_all <- grep(
        pattern = paste0(group, brm_sep(), subgroup),
        x = colnames(out$response),
        fixed = TRUE,
        value = TRUE
      )
      cols_sub <- grep(
        pattern = "time_2|time_3",
        x = cols_all,
        fixed = FALSE,
        value = TRUE
      )
      name <- name_marginal_subgroup(group, subgroup, "average")
      expect_equal(
        averages_all$response[[name]],
        apply(tibble::as_tibble(out$response)[, cols_all], 1, mean)
      )
      name <- name_marginal_subgroup(group, subgroup, "mean")
      expect_equal(
        averages_sub$response[[name]],
        apply(tibble::as_tibble(out$response)[, cols_sub], 1, mean)
      )
      cols_all <- intersect(cols_all, colnames(out$difference_time))
      cols_sub <- intersect(cols_sub, colnames(out$difference_time))
      name <- name_marginal_subgroup(group, subgroup, "average")
      expect_equal(
        averages_all$difference_time[[name]],
        apply(tibble::as_tibble(out$difference_time)[, cols_all], 1, mean)
      )
      name <- name_marginal_subgroup(group, subgroup, "mean")
      expect_equal(
        averages_sub$difference_time[[name]],
        apply(tibble::as_tibble(out$difference_time)[, cols_sub], 1, mean)
      )
      cols_all <- intersect(cols_all, colnames(out$difference_group))
      cols_sub <- intersect(cols_sub, colnames(out$difference_group))
      if (group != "group_1") {
        fields <- c("difference_group", "effect")
        if (subgroup != "subgroup_1") {
          fields <- c(fields, "difference_subgroup")
        }
        for (field in fields) {
          name <- name_marginal_subgroup(group, subgroup, "average")
          expect_equal(
            averages_all[[field]][[name]],
            apply(tibble::as_tibble(out[[field]])[, cols_all], 1, mean)
          )
          name <- name_marginal_subgroup(group, subgroup, "mean")
          expect_equal(
            averages_sub[[field]][[name]],
            apply(tibble::as_tibble(out[[field]])[, cols_sub], 1, mean)
          )
        }
      }
    }
  }
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_all))))
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_sub))))
  expect_true(is.list(brm_marginal_probabilities(averages_all)))
  expect_true(is.list(brm_marginal_probabilities(averages_sub)))
})
