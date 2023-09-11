test_that("brm_simulate_continuous()", {
  set.seed(0L)
  data <- brm_simulate_outline(n_patient = 1e4)
  mean <- 1070.25
  sd <- 99.5
  out <- brm_simulate_continuous(
    data = data,
    names = c("x", "y"),
    mean = mean,
    sd = sd
  )
  expect_equal(
    sort(attr(out, "brm_covariates")),
    sort(c("x", "y"))
  )
  for (field in c("x", "y")) {
    expect_equal(mean(out[[field]]), mean, tolerance = 0.01)
    expect_equal(sd(out[[field]]), sd, tolerance = 0.01)
  }
  expect_error(
    brm_simulate_continuous(data = out, names = c("x", "y")),
    class = "brm_error"
  )
})
