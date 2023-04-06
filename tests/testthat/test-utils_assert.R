test_that("true()", {
  expect_silent(true(TRUE))
  expect_error(true(FALSE), class = "brm_error")
  expect_silent(true(c(2, 3), . > 1, . > 0))
  expect_error(true(2, . < 1), class = "brm_error")
})

test_that("brm_error()", {
  expect_error(brm_error("message"), class = "brm_error")
})

test_that("brm_warn()", {
  expect_warning(brm_warn("message"), class = "brm_warn")
})
