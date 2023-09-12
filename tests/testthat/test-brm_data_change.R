test_that("brm_data_change()", {
  set.seed(0)
  data <- brm_data(
    data = dplyr::rename(brm_simulate_simple()$data, y_values = response),
    outcome = "y_values",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient",
    level_control = "group_1",
    level_baseline = "time_1"
  )
  expect_equal(attr(data, "brm_role"), "response")
  expect_equal(attr(data, "brm_outcome"), "y_values")
  expect_null(attr(data, "brm_baseline"))
  expect_equal(attr(data, "brm_level_baseline"), "time_1")
  changed <- brm_data_change(
    data = data,
    name_change = "delta",
    name_baseline = "base"
  )
  expect_equal(attr(changed, "brm_role"), "change")
  expect_equal(attr(changed, "brm_outcome"), "delta")
  expect_equal(attr(changed, "brm_baseline"), "base")
  expect_null(attr(changed, "brm_level_baseline"))
})

test_that("brm_data_change() assertions", {
  set.seed(0)
  data <- brm_data(
    data = dplyr::rename(brm_simulate_simple()$data, y_values = response),
    outcome = "y_values",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient",
    level_control = "group_1",
    level_baseline = "time_1"
  )
  expect_equal(attr(data, "brm_role"), "response")
  expect_equal(attr(data, "brm_outcome"), "y_values")
  expect_null(attr(data, "brm_baseline"))
  expect_equal(attr(data, "brm_level_baseline"), "time_1")
  already_changed <- brm_data_change(
    data = data,
    name_change = "delta",
    name_baseline = "base"
  )
  expect_error(
    brm_data_change(already_changed, name_change = "abc", name_baseline = "b"),
    class = "brm_error"
  )
  expect_error(
    brm_data_change(data, name_change = "time", name_baseline = "new_column"),
    class = "brm_error"
  )
  expect_error(
    brm_data_change(data, name_change = "new_column", name_baseline = "time"),
    class = "brm_error"
  )
})
