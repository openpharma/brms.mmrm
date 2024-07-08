test_that("brm_marginal_grid() non-subgroup", {
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  data <- brm_data(
    data = fev_data,
    outcome = "FEV1",
    role = "response",
    group = "ARMCD",
    time = "AVISIT",
    patient = "USUBJID",
    reference_group = "PBO",
    reference_time = "VIS1"
  )
  out <- brm_marginal_grid(data, brm_formula(data))
  expect_equal(
    sort(colnames(out)),
    sort(c("name", "group", "time"))
  )
  expect_equal(
    out$name,
    c(
      "PBO|VIS1",
      "PBO|VIS2",
      "PBO|VIS3",
      "PBO|VIS4",
      "TRT|VIS1",
      "TRT|VIS2",
      "TRT|VIS3",
      "TRT|VIS4"
    )
  )
  expect_equal(
    out$group,
    as.factor(rep(c("PBO", "TRT"), each = 4L))
  )
  expect_equal(
    out$time,
    as.factor(rep(paste0("VIS", seq_len(4L)), times = 2L))
  )
})

test_that("brm_marginal_grid() subgroup", {
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  data <- brm_data(
    data = fev_data,
    outcome = "FEV1",
    role = "response",
    group = "ARMCD",
    subgroup = "SEX",
    time = "AVISIT",
    patient = "USUBJID",
    reference_group = "PBO",
    reference_subgroup = "Female",
    reference_time = "VIS1"
  )
  out <- brm_marginal_grid(data, brm_formula(data))
  expect_equal(
    sort(colnames(out)),
    sort(c("name", "group", "subgroup", "time"))
  )
  expect_equal(
    out$name,
    c(
      "PBO|Male|VIS1",
      "PBO|Male|VIS2",
      "PBO|Male|VIS3",
      "PBO|Male|VIS4",
      "PBO|Female|VIS1",
      "PBO|Female|VIS2",
      "PBO|Female|VIS3",
      "PBO|Female|VIS4",
      "TRT|Male|VIS1",
      "TRT|Male|VIS2",
      "TRT|Male|VIS3",
      "TRT|Male|VIS4",
      "TRT|Female|VIS1",
      "TRT|Female|VIS2",
      "TRT|Female|VIS3",
      "TRT|Female|VIS4"
    )
  )
  expect_equal(out$group, as.factor(rep(c("PBO", "TRT"), each = 8L)))
  expect_equal(
    out$subgroup,
    factor(
      rep(rep(c("Male", "Female"), times = 2L), each = 4L),
      levels = c("Male", "Female")
    )
  )
  expect_equal(
    out$time,
    as.factor(rep(paste0("VIS", seq_len(4L)), times = 4L))
  )
})
