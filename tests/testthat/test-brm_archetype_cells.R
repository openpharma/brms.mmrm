test_that("brm_archetype_cells() change and non-subgroup", {
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
  out <- brm_archetype_cells(
    data,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  suppressMessages(expect_true(is.character(summary(out))))
  expect_true(is.character(summary(out, message = FALSE)))
  out2 <- brm_archetype_cells(
    out,
    prefix_interest = "y_",
    prefix_nuisance = "z_",
    baseline = TRUE,
    baseline_time = FALSE
  )
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_cells")
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
  expect_equal(unname(as.matrix(grid[, seq(3L, 8L)])), diag(6L))
})

test_that("brm_archetype_cells() non-change subgroup", {
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
  out <- brm_archetype_cells(data)
  out2 <- brm_archetype_cells(out)
  expect_equal(out, out2)
  expect_silent(brm_data_validate(out))
  expect_true(all(class(data) %in% class(out)))
  expect_s3_class(out, "brms_mmrm_cells")
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
  expect_equal(unname(as.matrix(grid[, seq(4L, 21L)])), diag(18L))
})

test_that("brm_archetype_cells() intercept non-subgroup", {
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
  out <- brm_archetype_cells(
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
  exp <- diag(6L)
  exp[, 1L] <- 1L
  expect_equal(unname(as.matrix(grid[, seq(3L, 8L)])), exp)
})

test_that("brm_archetype_cells() intercept subgroup", {
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
  out <- brm_archetype_cells(data, intercept = TRUE)
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
  exp <- diag(18L)
  exp[, 1L] <- 1L
  expect_equal(unname(as.matrix(grid[, seq(4L, 21L)])), exp)
})

test_that("brm_archetype_cells() clda non_intercept non-subgroup", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = 0)
  out <- brm_archetype_cells(data, clda = TRUE, intercept = FALSE)
  full <- brm_archetype_cells(data, clda = FALSE, intercept = FALSE)
  expect_equal(
    attr(out, "brm_archetype_interest"),
    setdiff(attr(full, "brm_archetype_interest"), "x_group_2_time_1")
  )
  mapping <- attr(out, "brm_archetype_mapping")
  mapping_full <- attr(full, "brm_archetype_mapping")
  expect_equal(
    mapping,
    mapping_full[mapping_full$variable != "x_group_2_time_1", ]
  )
  columns <- c("group", "time", attr(out, "brm_archetype_interest"))
  grid <- dplyr::arrange(dplyr::distinct(out[, columns]), group, time)
  grid2 <- dplyr::arrange(dplyr::distinct(full[, columns]), group, time)
  grid2[grid2$group == "group_2" & grid2$time == "time_1", ] <-
    grid2[grid2$group == "group_1" & grid2$time == "time_1", ]
  expect_equal(
    as.matrix(grid)[, attr(out, "brm_archetype_interest")],
    as.matrix(grid2)[, attr(out, "brm_archetype_interest")]
  )
  equations <- summary(out, message = FALSE)
  expected <- c(
    "group_1:time_1 = x_group_1_time_1",
    "group_1:time_2 = x_group_1_time_2",
    "group_1:time_3 = x_group_1_time_3",
    "group_1:time_4 = x_group_1_time_4",
    "group_2:time_1 = x_group_1_time_1",
    "group_2:time_2 = x_group_2_time_2",
    "group_2:time_3 = x_group_2_time_3",
    "group_2:time_4 = x_group_2_time_4"
  )
  expect_equal(equations, expected)
})

test_that("brm_archetype_cells() clda intercept non-subgroup", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = 0)
  out <- brm_archetype_cells(data, clda = TRUE, intercept = TRUE)
  full <- brm_archetype_cells(data, clda = FALSE, intercept = TRUE)
  expect_equal(
    attr(out, "brm_archetype_interest"),
    setdiff(attr(full, "brm_archetype_interest"), "x_group_2_time_1")
  )
  mapping <- attr(out, "brm_archetype_mapping")
  mapping_full <- attr(full, "brm_archetype_mapping")
  expect_equal(
    mapping,
    mapping_full[mapping_full$variable != "x_group_2_time_1", ]
  )
  columns <- c("group", "time", attr(out, "brm_archetype_interest"))
  grid <- dplyr::arrange(dplyr::distinct(out[, columns]), group, time)
  grid2 <- dplyr::arrange(dplyr::distinct(full[, columns]), group, time)
  grid2[grid2$group == "group_2" & grid2$time == "time_1", ] <-
    grid2[grid2$group == "group_1" & grid2$time == "time_1", ]
  expect_equal(
    as.matrix(grid)[, attr(out, "brm_archetype_interest")],
    as.matrix(grid2)[, attr(out, "brm_archetype_interest")]
  )
  equations <- summary(out, message = FALSE)
  expected <- c(
    "group_1:time_1 = x_group_1_time_1",
    "group_1:time_2 = x_group_1_time_1 + x_group_1_time_2",
    "group_1:time_3 = x_group_1_time_1 + x_group_1_time_3",
    "group_1:time_4 = x_group_1_time_1 + x_group_1_time_4",
    "group_2:time_1 = x_group_1_time_1",
    "group_2:time_2 = x_group_1_time_1 + x_group_2_time_2",
    "group_2:time_3 = x_group_1_time_1 + x_group_2_time_3",
    "group_2:time_4 = x_group_1_time_1 + x_group_2_time_4"
  )
  expect_equal(equations, expected)
})

test_that("brm_archetype_cells() clda subgroup", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 2,
    n_patient = 100,
    n_time = 2,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = 0)
  out <- brm_archetype_cells(data, clda = TRUE)
  full <- brm_archetype_cells(data, clda = FALSE)
  dropped <- c("x_group_2_subgroup_1_time_1", "x_group_2_subgroup_2_time_1")
  expect_equal(
    attr(out, "brm_archetype_interest"),
    setdiff(
      attr(full, "brm_archetype_interest"),
      dropped
    )
  )
  mapping <- attr(out, "brm_archetype_mapping")
  mapping_full <- attr(full, "brm_archetype_mapping")
  expect_equal(
    mapping,
    mapping_full[!(mapping_full$variable %in% dropped), ]
  )
  columns <- c(
    "group", "subgroup", "time",
    attr(out, "brm_archetype_interest")
  )
  grid <- dplyr::arrange(dplyr::distinct(out[, columns]), group, time)
  grid2 <- dplyr::arrange(dplyr::distinct(full[, columns]), group, time)
  for (subgroup in c("subgroup_1", "subgroup_2")) {
    index1 <- grid2$group == "group_2" &
      grid2$subgroup == subgroup &
      grid2$time == "time_1"
    index2 <- grid2$group == "group_1" &
      grid2$subgroup == subgroup &
      grid2$time == "time_1"
    grid2[index1, ] <- grid2[index2, ]
  }
  expect_equal(
    as.matrix(grid)[, attr(out, "brm_archetype_interest")],
    as.matrix(grid2)[, attr(out, "brm_archetype_interest")]
  )
  equations <- summary(out, message = FALSE)
  expected <- c(
    "group_1:subgroup_1:time_1 = x_group_1_subgroup_1_time_1",
    "group_1:subgroup_1:time_2 = x_group_1_subgroup_1_time_2",
    "group_1:subgroup_2:time_1 = x_group_1_subgroup_2_time_1",
    "group_1:subgroup_2:time_2 = x_group_1_subgroup_2_time_2",
    "group_2:subgroup_1:time_1 = x_group_1_subgroup_1_time_1",
    "group_2:subgroup_1:time_2 = x_group_2_subgroup_1_time_2",
    "group_2:subgroup_2:time_1 = x_group_1_subgroup_2_time_1",
    "group_2:subgroup_2:time_2 = x_group_2_subgroup_2_time_2"
  )
  expect_equal(equations, expected)
})
