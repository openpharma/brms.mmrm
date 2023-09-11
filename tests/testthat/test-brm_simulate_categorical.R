test_that("brm_simulate_categorical() values", {
  set.seed(0L)
  data <- brm_simulate_outline(n_patient = 1e4)
  out <- brm_simulate_categorical(
    data = data,
    names = c("site", "region"),
    levels = c("area1", "area2")
  )
  expect_equal(
    sort(attr(out, "brm_covariates")),
    sort(c("site", "region"))
  )
  for (field in c("site", "region")) {
    for (value in c("area1", "area2")) {
      expect_equal(mean(out[[field]] == value), 0.5, tolerance = 0.01)
    }
  }
  out <- brm_simulate_categorical(
    data = data,
    names = c("site", "region"),
    levels = c("area1", "area2"),
    probabilities = c(0.57, 0.43)
  )
  for (field in c("site", "region")) {
    expect_equal(mean(out[[field]] == "area1"), 0.57, tolerance = 0.01)
    expect_equal(mean(out[[field]] == "area2"), 0.43, tolerance = 0.01)
  }
  expect_error(
    brm_simulate_categorical(
      data = out,
      names = c("site", "region"),
      levels = "a"
    ),
    class = "brm_error"
  )
  expect_error(
    brm_simulate_categorical(
      data = out,
      names = c("site2", "region2"),
      levels = c("a", "b"),
      probabilities = c(0.7, 0.7)
    ),
    class = "brm_error"
  )
})

test_that("brm_simulate_categorical() values", {
  set.seed(0L)
  data <- brm_simulate_outline(n_patient = 1e4, n_time = 7L)
  data <- brm_simulate_categorical(
    data = data,
    names = "site",
    levels = c("area1", "area2")
  )
  for (value in c("area1", "area2")) {
    expect_equal(mean(data$site == value), 0.5, tolerance = 0.01)
  }
  out <- tapply(
    X = data$site,
    INDEX = data$patient,
    FUN = function(x) {
      length(unique(x)) == 1L
    }
  )
  expect_true(all(out))
})
