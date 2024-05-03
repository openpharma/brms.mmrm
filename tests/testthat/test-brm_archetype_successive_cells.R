test_that("brm_archetype_successive_cells() change and non-subgroup", {
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
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  tmp <- capture.output(summary(out))
  out2 <- brm_archetype_successive_cells(
    out,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_successive_cells")
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
  expect_equal(
    unname(as.matrix(grid[, seq(3L, 8L)])),
    kronecker(diag(2), diag(3) + lower.tri(diag(3)))
  )
})

test_that("brm_archetype_successive_cells() change non-sub options", {
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
  interest_exp <- sort(
    c(
      "y_group_1_time_2",
      "y_group_1_time_3",
      "y_group_1_time_4",
      "y_group_2_time_2",
      "y_group_2_time_3",
      "y_group_2_time_4"
    )
  )
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = FALSE,
    baseline_time = TRUE,
    covariates = TRUE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(attr(out, "brm_archetype_interest")), interest_exp)
  expect_equal(
    sort(attr(out, "brm_archetype_nuisance")),
    sort(
      c(
        "z_biomarker1",
        "z_biomarker2",
        "z_baseline.timetime_2",
        "z_baseline.timetime_3",
        "z_baseline.timetime_4",
        "z_status1_absent",
        "z_status2_present"
      )
    )
  )
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = FALSE,
    baseline_time = TRUE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(attr(out, "brm_archetype_interest")), interest_exp)
  expect_equal(
    sort(attr(out, "brm_archetype_nuisance")),
    sort(
      c(
        "z_baseline.timetime_2",
        "z_baseline.timetime_3",
        "z_baseline.timetime_4"
      )
    )
  )
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(attr(out, "brm_archetype_interest")), interest_exp)
  expect_equal(
    sort(attr(out, "brm_archetype_nuisance")),
    "z_baseline"
  )
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = FALSE,
    baseline_time = FALSE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(sort(attr(out, "brm_archetype_interest")), interest_exp)
  expect_equal(attr(out, "brm_archetype_nuisance"), character(0L))
  out <- brm_archetype_successive_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = FALSE,
    baseline_time = FALSE,
    covariates = TRUE
  )
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(attr(out, "brm_archetype_interest")), interest_exp)
  expect_equal(
    attr(out, "brm_archetype_nuisance"),
    sort(
      c(
        "z_biomarker1",
        "z_biomarker2",
        "z_status1_absent",
        "z_status2_present"
      )
    )
  )
})

test_that("brm_archetype_successive_cells() non-change subgroup", {
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
  out <- brm_archetype_successive_cells(data)
  out2 <- brm_archetype_successive_cells(out)
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_successive_cells")
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
  expect_equal(
    unname(as.matrix(grid[, seq(4L, 21L)])),
    kronecker(diag(6L), diag(3L) + lower.tri(diag(3L)))
  )
})

test_that("brm_archetype_successive_cells() baseline-subgroup interactions", {
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
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  interest_exp <- sort(
    c(
      "x_group_1_subgroup_1_time_2",
      "x_group_1_subgroup_1_time_3",
      "x_group_1_subgroup_2_time_2",
      "x_group_1_subgroup_2_time_3",
      "x_group_1_subgroup_3_time_2",
      "x_group_1_subgroup_3_time_3",
      "x_group_2_subgroup_1_time_2",
      "x_group_2_subgroup_1_time_3",
      "x_group_2_subgroup_2_time_2",
      "x_group_2_subgroup_2_time_3",
      "x_group_2_subgroup_3_time_2",
      "x_group_2_subgroup_3_time_3"
    )
  )
  out <- brm_archetype_successive_cells(
    data,
    baseline = FALSE,
    baseline_subgroup = FALSE,
    baseline_subgroup_time = FALSE,
    baseline_time = FALSE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(sort(interest), interest_exp)
  expect_equal(sort(nuisance), character(0L))
  out <- brm_archetype_successive_cells(
    data,
    baseline = FALSE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = FALSE,
    baseline_time = FALSE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(interest), interest_exp)
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "nuisance_baseline.subgroupsubgroup_1",
        "nuisance_baseline.subgroupsubgroup_2",
        "nuisance_baseline.subgroupsubgroup_3"
      )
    )
  )
  out <- brm_archetype_successive_cells(
    data,
    baseline = FALSE,
    baseline_subgroup = FALSE,
    baseline_subgroup_time = TRUE,
    baseline_time = FALSE,
    covariates = FALSE
  )
  interest <- attr(out, "brm_archetype_interest")
  nuisance <- attr(out, "brm_archetype_nuisance")
  expect_equal(
    qr(out[, c(interest, nuisance), drop = FALSE])$rank,
    length(interest) + length(nuisance)
  )
  expect_equal(max(abs(colMeans(out[, nuisance]))), 0)
  expect_equal(sort(interest), interest_exp)
  expect_equal(
    sort(nuisance),
    sort(
      c(
        "nuisance_baseline.subgroupsubgroup_1.timetime_2",
        "nuisance_baseline.subgroupsubgroup_1.timetime_3",
        "nuisance_baseline.subgroupsubgroup_2.timetime_2",
        "nuisance_baseline.subgroupsubgroup_2.timetime_3",
        "nuisance_baseline.subgroupsubgroup_3.timetime_2",
        "nuisance_baseline.subgroupsubgroup_3.timetime_3"
      )
    )
  )
})
