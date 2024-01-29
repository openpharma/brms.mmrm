test_that("brm_data() response", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  for (field in c("group", "time", "patient")) {
    data[[field]] <- gsub("_", " ", data[[field]])
  }
  data$group <- as.factor(data$group)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  data$col_missing <- FALSE
  out <- brm_data(
    data = data,
    outcome = "col_response",
    role = "response",
    group = "col_group",
    time = "col_time",
    patient = "col_patient",
    covariates = c("col_factor2", "col_factor3"),
    reference_group = "group 1",
    reference_time = "time 1",
    missing = "col_missing"
  )
  expect_s3_class(out, "brm_data")
  expect_true(tibble::is_tibble(out))
  expect_silent(brm_data_validate(out))
  expect_true(all(is.na(out$col_response[c(2L, 3L)])))
  expect_false(anyNA(out$col_response[- c(2L, 3L)]))
  expect_equal(nrow(out), 800L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "col_patient",
        "col_time",
        "col_response",
        "col_group",
        "col_factor2",
        "col_factor3",
        "col_missing"
      )
    )
  )
  expect_equal(
    out$col_group,
    rep(paste0("group.", c(1L, 2L)), each = 400L)
  )
  expect_equal(
    out$col_time,
    rep(paste0("time.", seq_len(4L)), times = 200L)
  )
  expect_equal(
    sort(out$col_response[- c(2L, 3L)]),
    sort(c(data$col_response))
  )
  expect_equal(out$col_patient, out$col_factor2)
  expect_equal(out$col_patient, out$col_factor3)
  expect_equal(attr(out, "brm_outcome"), "col_response")
  expect_equal(attr(out, "brm_role"), "response")
  expect_equal(attr(out, "brm_group"), "col_group")
  expect_null(attr(out, "brm_subgroup"))
  expect_equal(attr(out, "brm_time"), "col_time")
  expect_equal(attr(out, "brm_patient"), "col_patient")
  expect_equal(attr(out, "brm_covariates"), c("col_factor2", "col_factor3"))
  expect_equal(
    sort(attr(out, "brm_levels_group")), c("group.1", "group.2")
  )
  expect_null(attr(out, "brm_levels_subgroup"))
  expect_equal(
    sort(attr(out, "brm_levels_time")), paste0("time.", seq_len(4L))
  )
  expect_equal(
    sort(attr(out, "brm_labels_group")), c("group 1", "group 2")
  )
  expect_null(attr(out, "brm_labels_subgroup"))
  expect_equal(
    sort(attr(out, "brm_labels_time")), paste("time", seq_len(4L))
  )
  expect_equal(attr(out, "brm_missing"), "col_missing")
  expect_equal(attr(out, "brm_reference_group"), "group.1")
  expect_equal(attr(out, "brm_reference_time"), "time.1")
})

test_that("brm_data() response with subgroup", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  data <- tidyr::expand_grid(data, subgroup = c("subgroup 1", "subgroup 2"))
  data$patient <- paste(data$patient, data$subgroup)
  for (field in c("group", "subgroup", "time", "patient")) {
    data[[field]] <- gsub("_", " ", data[[field]])
  }
  data$group <- as.factor(data$group)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  slice_missing <- dplyr::arrange(data[c(2L, 3L), ], col_time)
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  data$col_missing <- FALSE
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "col_group",
      subgroup = "col_subgroup",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3"),
      reference_group = "group 1",
      reference_time = "time 1",
      missing = "col_missing"
    ),
    class = "brm_error"
  )
  out <- brm_data(
    data = data,
    outcome = "col_response",
    role = "response",
    group = "col_group",
    subgroup = "col_subgroup",
    time = "col_time",
    patient = "col_patient",
    covariates = c("col_factor2", "col_factor3"),
    reference_group = "group 1",
    reference_subgroup = "subgroup 1",
    reference_time = "time 1",
    missing = "col_missing"
  )
  expect_s3_class(out, "brm_data")
  expect_true(tibble::is_tibble(out))
  expect_silent(brm_data_validate(out))
  slice_filled <- dplyr::arrange(out[is.na(out$col_response), ], col_time)
  expect_equal(nrow(slice_filled), 2L)
  fields <- c(
    c(
      "col_group",
      "col_subgroup",
      "col_time",
      "col_patient",
      paste0("col_factor", seq_len(3L))
    )
  )
  for (field in c("col_group", "col_subgroup", "col_time", "col_patient")) {
    slice_missing[[field]] <- gsub(
      pattern = " ",
      replacement = "_",
      as.character(slice_missing[[field]])
    )
    for (pattern in c(".", " ")) {
      slice_filled[[field]] <- gsub(
        pattern = pattern,
        replacement = "_",
        as.character(slice_filled[[field]]),
        fixed = TRUE
      )
    }
    expect_equal(slice_missing[[field]], slice_filled[[field]])
  }
  expect_equal(slice_filled$col_missing, c(FALSE, FALSE))
  expect_equal(nrow(out), 1600L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "col_patient",
        "col_time",
        "col_response",
        "col_group",
        "col_subgroup",
        "col_factor2",
        "col_factor3",
        "col_missing"
      )
    )
  )
  expect_equal(
    out$col_group,
    rep(paste0("group.", c(1L, 2L)), each = 800L)
  )
  expect_equal(
    out$col_subgroup,
    rep(paste0("subgroup.", c(1L, 2L, 1L, 2L)), each = 400L)
  )
  expect_equal(
    out$col_time,
    rep(paste0("time.", seq_len(4L)), times = 400L)
  )
  expect_equal(out$col_patient, out$col_factor2)
  expect_equal(out$col_patient, out$col_factor3)
  expect_equal(attr(out, "brm_outcome"), "col_response")
  expect_equal(attr(out, "brm_role"), "response")
  expect_equal(attr(out, "brm_group"), "col_group")
  expect_equal(attr(out, "brm_subgroup"), "col_subgroup")
  expect_equal(attr(out, "brm_time"), "col_time")
  expect_equal(attr(out, "brm_patient"), "col_patient")
  expect_equal(attr(out, "brm_covariates"), c("col_factor2", "col_factor3"))
  expect_equal(
    sort(attr(out, "brm_levels_group")), c("group.1", "group.2")
  )
  expect_equal(
    sort(attr(out, "brm_levels_subgroup")), c("subgroup.1", "subgroup.2")
  )
  expect_equal(
    sort(attr(out, "brm_levels_time")), paste0("time.", seq_len(4L))
  )
  expect_equal(
    sort(attr(out, "brm_labels_group")), c("group 1", "group 2")
  )
  expect_equal(
    sort(attr(out, "brm_labels_subgroup")), c("subgroup 1", "subgroup 2")
  )
  expect_equal(
    sort(attr(out, "brm_labels_time")), paste("time", seq_len(4L))
  )
  expect_equal(attr(out, "brm_missing"), "col_missing")
  expect_equal(attr(out, "brm_reference_group"), "group.1")
  expect_equal(attr(out, "brm_reference_subgroup"), "subgroup.1")
  expect_equal(attr(out, "brm_reference_time"), "time.1")
})

test_that("brm_data() change", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  for (field in c("group", "time", "patient")) {
    data[[field]] <- gsub("_", " ", data[[field]])
  }
  data$group <- as.factor(data$group)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  out <- brm_data(
    data = data,
    outcome = "col_response",
    role = "change",
    group = "col_group",
    time = "col_time",
    patient = "col_patient",
    covariates = character(0L),
    reference_group = "group 1"
  )
  expect_s3_class(out, "brm_data")
  expect_true(tibble::is_tibble(out))
  expect_silent(brm_data_validate(out))
  expect_true(all(is.na(out$col_response[c(2L, 3L)])))
  expect_false(anyNA(out$col_response[- c(2L, 3L)]))
  expect_equal(nrow(out), 800L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "col_patient",
        "col_time",
        "col_response",
        "col_group"
      )
    )
  )
  expect_equal(
    out$col_group,
    rep(paste0("group.", c(1L, 2L)), each = 400L)
  )
  expect_equal(
    out$col_time,
    rep(paste0("time.", seq_len(4L)), times = 200L)
  )
  expect_equal(
    sort(out$col_response[- c(2L, 3L)]),
    sort(c(data$col_response))
  )
  expect_equal(attr(out, "brm_outcome"), "col_response")
  expect_equal(attr(out, "brm_role"), "change")
  expect_equal(attr(out, "brm_group"), "col_group")
  expect_equal(attr(out, "brm_time"), "col_time")
  expect_equal(attr(out, "brm_patient"), "col_patient")
  expect_equal(attr(out, "brm_covariates"), character(0L))
  expect_equal(
    sort(attr(out, "brm_levels_group")), c("group.1", "group.2")
  )
  expect_equal(
    sort(attr(out, "brm_levels_time")), paste0("time.", seq_len(4L))
  )
  expect_equal(
    sort(attr(out, "brm_labels_group")), c("group 1", "group 2")
  )
  expect_equal(
    sort(attr(out, "brm_labels_time")), paste("time", seq_len(4L))
  )
  expect_null(attr(out, "brm_missing"))
  expect_equal(attr(out, "brm_reference_group"), "group.1")
  expect_null(attr(out, "brm_reference_time"))
})

test_that("brm_data() bad role", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "response",
      role = "nope",
      group = "col_group",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3"),
      reference_group = "group 1",
      reference_time = "time 1"
    ),
    class = "brm_error"
  )
})

test_that("brm_data() bad group", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "nope",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3"),
      reference_group = "group 1",
      reference_time = "time 1"
    ),
    class = "brm_error"
  )
})

test_that("brm_data() levels ", {
  set.seed(0)
  sim <- brm_simulate_simple()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "nope",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})

test_that("brm_levels()", {
  expect_equal(brm_levels(c("a 1", "a 1", "b 2")), c("a.1", "a.1", "b.2"))
  expect_error(brm_levels(c("a 1", "a.1")), class = "brm_error")
})

test_that("brm_data() deprecate level_control", {
  set.seed(0)
  expect_warning(
    brm_data(
      data = brm_simulate_simple()$data,
      outcome = "response",
      role = "response",
      group = "group",
      time = "time",
      patient = "patient",
      level_control = "group_1",
      reference_time = "time_1"
    ),
    class = "brm_deprecate"
  )
})

test_that("brm_data() deprecate level_baseline", {
  set.seed(0)
  expect_warning(
    brm_data(
      data = brm_simulate_simple()$data,
      outcome = "response",
      role = "response",
      group = "group",
      time = "time",
      patient = "patient",
      reference_group = "group_1",
      level_baseline = "time_1"
    ),
    class = "brm_deprecate"
  )
})
