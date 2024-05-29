test_that("brm_prior_label non-subgroup", {
  label <- brm_prior_label(
    code = "normal(1, 1)",
    group = "group_1",
    time = "time_1"
  ) |>
    brm_prior_label("normal(1, 2)", group = "group_1", time = "time_2") |>
    brm_prior_label("normal(2, 1)", group = "group_2", time = "time_1")
  expect_equal(sort(colnames(label)), sort(c("code", "group", "time")))
  expect_equal(label$code, c("normal(1, 1)", "normal(1, 2)", "normal(2, 1)"))
  expect_equal(label$group, c("group_1", "group_1", "group_2"))
  expect_equal(label$time, c("time_1", "time_2", "time_1"))
})

test_that("brm_prior_label subgroup", {
  label <- brm_prior_label(
    code = "normal(1, 1)",
    group = 1,
    subgroup = 1,
    time = 1
  ) |>
    brm_prior_label("normal(1, 2)", group = 1, subgroup = 3, time = 2) |>
    brm_prior_label("normal(2, 1)", group = 2, subgroup = 4, time = 1)
  expect_equal(
    sort(colnames(label)),
    sort(c("code", "group", "subgroup", "time"))
  )
  expect_equal(label$code, c("normal(1, 1)", "normal(1, 2)", "normal(2, 1)"))
  expect_equal(label$group, c(1, 1, 2))
  expect_equal(label$subgroup, c(1, 3, 4))
  expect_equal(label$time, c(1, 2, 1))
})
