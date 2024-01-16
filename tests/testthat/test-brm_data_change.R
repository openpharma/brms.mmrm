test_that("brm_data_change()", {
  set.seed(0)
  data <- dplyr::rename(brm_simulate_simple()$data, y_values = response)
  data$missing <- sample(c(TRUE, FALSE), size = nrow(data), replace = TRUE)
  data <- brm_data(
    data = data,
    outcome = "y_values",
    role = "response",
    group = "group",
    time = "time",
    missing = "missing",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  expect_equal(attr(data, "brm_role"), "response")
  expect_equal(attr(data, "brm_outcome"), "y_values")
  expect_null(attr(data, "brm_baseline"))
  expect_equal(attr(data, "brm_reference_time"), "time_1")
  expect_equal(attr(data, "brm_missing"), "missing")
  changed <- brm_data_change(
    data = data,
    name_change = "delta",
    name_baseline = "base"
  )
  base <- data[data$time == "time_1", ]
  for (time in unique(changed$time)) {
    sub1 <- data[data$time == time, ]
    sub2 <- changed[changed$time == time, ]
    expect_true(all(sub2$missing == base$missing | sub1$missing))
  }
  expect_equal(attr(changed, "brm_role"), "change")
  expect_equal(attr(changed, "brm_outcome"), "delta")
  expect_equal(attr(changed, "brm_baseline"), "base")
  expect_null(attr(changed, "brm_reference_time"))
  data_baseline <- dplyr::filter(data, time == "time_1")
  data_after <- dplyr::filter(data, time != "time_1")
  for (point in setdiff(unique(data$time), "time_1")) {
    expect_equal(
      as.numeric(changed[changed$time == point, ]$base),
      as.numeric(data[data$time == "time_1", ]$y_values)
    )
    y_post <- data[data$time == point, ]$y_values
    y_base <- data[data$time == "time_1", ]$y_values
    expect_equal(
      as.numeric(changed[changed$time == point, ]$delta),
      as.numeric(y_post - y_base)
    )
  }
})

test_that("brm_data_change()", {
  set.seed(0)
  data <- brm_data(
    data = dplyr::rename(brm_simulate_simple()$data, y_values = response),
    outcome = "y_values",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  expect_equal(attr(data, "brm_role"), "response")
  expect_equal(attr(data, "brm_outcome"), "y_values")
  expect_null(attr(data, "brm_baseline"))
  expect_equal(attr(data, "brm_reference_time"), "time_1")
  changed <- brm_data_change(
    data = data,
    name_change = "delta",
    name_baseline = "base"
  )
  expect_equal(attr(changed, "brm_role"), "change")
  expect_equal(attr(changed, "brm_outcome"), "delta")
  expect_equal(attr(changed, "brm_baseline"), "base")
  expect_null(attr(changed, "brm_reference_time"))
  data_baseline <- dplyr::filter(data, time == "time_1")
  data_after <- dplyr::filter(data, time != "time_1")
  for (point in setdiff(unique(data$time), "time_1")) {
    expect_equal(
      as.numeric(changed[changed$time == point, ]$base),
      as.numeric(data[data$time == "time_1", ]$y_values)
    )
    y_post <- data[data$time == point, ]$y_values
    y_base <- data[data$time == "time_1", ]$y_values
    expect_equal(
      as.numeric(changed[changed$time == point, ]$delta),
      as.numeric(y_post - y_base)
    )
  }
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
    reference_group = "group_1",
    reference_time = "time_1"
  )
  expect_equal(attr(data, "brm_role"), "response")
  expect_equal(attr(data, "brm_outcome"), "y_values")
  expect_null(attr(data, "brm_baseline"))
  expect_equal(attr(data, "brm_reference_time"), "time_1")
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
