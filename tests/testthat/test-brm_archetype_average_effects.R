test_that("brm_archetype_average_effects() change and non-subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  out <- brm_archetype_average_effects(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  tmp <- suppressMessages(summary(out))
  out2 <- brm_archetype_average_effects(
    out,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_average_effects")
  expect_s3_class(out, "brms_mmrm_archetype")
  attributes_data <- brm_data_attributes(data)
  attributes_archetype <- brm_data_attributes(out)
  attributes_archetype$brm_archetype_mapping <- NULL
  attributes_archetype$brm_archetype_interest <- NULL
  attributes_archetype$brm_archetype_nuisance <- NULL
  expect_equal(attributes_data, attributes_archetype)
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(
    sort(interest),
    sort(
      c(
        "y_group_1_time_2",
        "y_group_1_time_3",
        "y_group_1_time_4",
        "y_group_2_time_2",
        "y_group_2_time_3",
        "y_group_2_time_4"
      )
    )
  )
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "z_biomarker1",
        "z_biomarker2",
        "z_baseline",
        "z_status1_absent",
        "z_status2_present"
      )
    )
  )
  expect_true(all(interest %in% colnames(out)))
  expect_true(all(nuisance %in% colnames(out)))
  expect_equal(
    sort(interest),
    sort(grep("y_", colnames(out), value = TRUE))
  )
  expect_equal(
    sort(nuisance),
    sort(grep("z_", colnames(out), value = TRUE))
  )
  param <- attr(out, "brm_archetype_mapping")
  expect_equal(param$variable, interest)
  expect_equal(param$group, rep(c("group_1", "group_2"), each = 3L))
  expect_equal(param$time, rep(c("time_2", "time_3", "time_4"), times = 2L))
  grid <- dplyr::distinct(
    out,
    group,
    time,
    y_group_1_time_2,
    y_group_1_time_3,
    y_group_1_time_4,
    y_group_2_time_2,
    y_group_2_time_3,
    y_group_2_time_4
  ) |>
    dplyr::arrange(group, time)
  expect_equal(nrow(grid), 6L)
  expect_equal(grid$group, rep(c("group_1", "group_2"), each = 3L))
  expect_equal(grid$time, rep(paste0("time_", c(2L, 3L, 4L)), times = 2L))
  exp <- diag(3L)
  exp[1L, c(1L, 2L, 3L)] <- c(3L, -1L, -1L)
  exp <- kronecker(rbind(c(1L, 0L), c(1L, 1L)), exp)
  expect_equal(unname(as.matrix(grid[, seq(3L, 8L)])), exp)
})

test_that("brm_archetype_average_effects() non-change subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 3,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  out <- brm_archetype_average_effects(data)
  out2 <- brm_archetype_average_effects(out)
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_average_effects")
  expect_s3_class(out, "brms_mmrm_archetype")
  attributes_data <- brm_data_attributes(data)
  attributes_archetype <- brm_data_attributes(out)
  attributes_archetype$brm_archetype_mapping <- NULL
  attributes_archetype$brm_archetype_interest <- NULL
  attributes_archetype$brm_archetype_nuisance <- NULL
  expect_equal(attributes_data, attributes_archetype)
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(
    sort(interest),
    sort(
      c(
        "x_group_1_subgroup_1_time_1",
        "x_group_1_subgroup_1_time_2",
        "x_group_1_subgroup_1_time_3",
        "x_group_1_subgroup_2_time_1",
        "x_group_1_subgroup_2_time_2",
        "x_group_1_subgroup_2_time_3",
        "x_group_1_subgroup_3_time_1",
        "x_group_1_subgroup_3_time_2",
        "x_group_1_subgroup_3_time_3",
        "x_group_2_subgroup_1_time_1",
        "x_group_2_subgroup_1_time_2",
        "x_group_2_subgroup_1_time_3",
        "x_group_2_subgroup_2_time_1",
        "x_group_2_subgroup_2_time_2",
        "x_group_2_subgroup_2_time_3",
        "x_group_2_subgroup_3_time_1",
        "x_group_2_subgroup_3_time_2",
        "x_group_2_subgroup_3_time_3"
      )
    )
  )
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "nuisance_biomarker1",
        "nuisance_biomarker2",
        "nuisance_status1_absent",
        "nuisance_status2_present"
      )
    )
  )
  expect_true(all(interest %in% colnames(out)))
  expect_true(all(nuisance %in% colnames(out)))
  expect_equal(
    sort(interest),
    sort(grep("x_", colnames(out), value = TRUE))
  )
  expect_equal(
    sort(nuisance),
    sort(grep("nuisance_", colnames(out), value = TRUE))
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  param <- attr(out, "brm_archetype_mapping")
  expect_equal(param$variable, interest)
  expect_equal(param$group, rep(c("group_1", "group_2"), each = 9L))
  expect_equal(
    param$subgroup,
    rep(
      rep(c("subgroup_1", "subgroup_2", "subgroup_3"), each = 3L),
      times = 2L
    )
  )
  expect_equal(param$time, rep(c("time_1", "time_2", "time_3"), times = 6L))
  grid <- dplyr::distinct(
    out,
    group,
    subgroup,
    time,
    x_group_1_subgroup_1_time_1,
    x_group_1_subgroup_1_time_2,
    x_group_1_subgroup_1_time_3,
    x_group_1_subgroup_2_time_1,
    x_group_1_subgroup_2_time_2,
    x_group_1_subgroup_2_time_3,
    x_group_1_subgroup_3_time_1,
    x_group_1_subgroup_3_time_2,
    x_group_1_subgroup_3_time_3,
    x_group_2_subgroup_1_time_1,
    x_group_2_subgroup_1_time_2,
    x_group_2_subgroup_1_time_3,
    x_group_2_subgroup_2_time_1,
    x_group_2_subgroup_2_time_2,
    x_group_2_subgroup_2_time_3,
    x_group_2_subgroup_3_time_1,
    x_group_2_subgroup_3_time_2,
    x_group_2_subgroup_3_time_3
  ) |>
    dplyr::arrange(group, subgroup, time)
  expect_equal(nrow(grid), 18L)
  expect_equal(grid$group, rep(c("group_1", "group_2"), each = 9L))
  expect_equal(
    grid$subgroup,
    rep(rep(paste0("subgroup_", seq_len(3L)), each = 3L), times = 2L)
  )
  expect_equal(grid$time, rep(paste0("time_", seq_len(3L)), times = 6L))
  exp <- diag(3L)
  exp[1L, c(1L, 2L, 3L)] <- c(3L, -1L, -1L)
  exp <- kronecker(diag(3L), exp)
  exp <- kronecker(rbind(c(1L, 0L), c(1L, 1L)), exp)
  expect_equal(unname(as.matrix(grid[, seq(4L, 21L)])), exp)
})

test_that("brm_archetype_average_effects() change and non-subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  out <- brm_archetype_average_effects(
    data,
    intercept = TRUE,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  grid <- dplyr::distinct(
    out,
    group,
    time,
    y_group_1_time_2,
    y_group_1_time_3,
    y_group_1_time_4,
    y_group_2_time_2,
    y_group_2_time_3,
    y_group_2_time_4
  ) |>
    dplyr::arrange(group, time)
  expect_equal(nrow(grid), 6L)
  expect_equal(grid$group, rep(c("group_1", "group_2"), each = 3L))
  expect_equal(grid$time, rep(paste0("time_", c(2L, 3L, 4L)), times = 2L))
  exp <- diag(3L)
  exp[1L, c(1L, 2L, 3L)] <- c(3L, -1L, -1L)
  exp <- kronecker(rbind(c(1L, 0L), c(1L, 1L)), exp)
  exp[, 1L] <- 1L
  expect_equal(unname(as.matrix(grid[, seq(3L, 8L)])), exp)
})

test_that("brm_archetype_average_effects() intercept subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 3,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  out <- brm_archetype_average_effects(data, intercept = TRUE)
  grid <- dplyr::distinct(
    out,
    group,
    subgroup,
    time,
    x_group_1_subgroup_1_time_1,
    x_group_1_subgroup_1_time_2,
    x_group_1_subgroup_1_time_3,
    x_group_1_subgroup_2_time_1,
    x_group_1_subgroup_2_time_2,
    x_group_1_subgroup_2_time_3,
    x_group_1_subgroup_3_time_1,
    x_group_1_subgroup_3_time_2,
    x_group_1_subgroup_3_time_3,
    x_group_2_subgroup_1_time_1,
    x_group_2_subgroup_1_time_2,
    x_group_2_subgroup_1_time_3,
    x_group_2_subgroup_2_time_1,
    x_group_2_subgroup_2_time_2,
    x_group_2_subgroup_2_time_3,
    x_group_2_subgroup_3_time_1,
    x_group_2_subgroup_3_time_2,
    x_group_2_subgroup_3_time_3
  ) |>
    dplyr::arrange(group, subgroup, time)
  expect_equal(nrow(grid), 18L)
  expect_equal(grid$group, rep(c("group_1", "group_2"), each = 9L))
  expect_equal(
    grid$subgroup,
    rep(rep(paste0("subgroup_", seq_len(3L)), each = 3L), times = 2L)
  )
  expect_equal(grid$time, rep(paste0("time_", seq_len(3L)), times = 6L))
  exp <- diag(3L)
  exp[1L, c(1L, 2L, 3L)] <- c(3L, -1L, -1L)
  exp <- kronecker(diag(3L), exp)
  exp <- kronecker(rbind(c(1L, 0L), c(1L, 1L)), exp)
  exp[, 1L] <- 1L
  expect_equal(unname(as.matrix(grid[, seq(4L, 21L)])), exp)
})
