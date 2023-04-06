test_that("assert()", {
  expect_silent(assert(TRUE))
  expect_error(assert(FALSE), class = "brm_error")
  expect_silent(assert(c(2, 3), . > 1, . > 0))
  expect_error(assert(2, . < 1), class = "brm_error")
})

test_that("assert_num()", {
  expect_silent(assert_num(1.1))
  expect_silent(assert_num(-1L))
  expect_error(assert_num(c(1.1, 2.2)))
  expect_error(assert_num(numeric(0L)))
  expect_error(assert_num(NA_real_))
  expect_error(assert_num("1"))
})

test_that("assert_pos()", {
  expect_silent(assert_pos(1.1))
  expect_error(assert_pos(-1L))
  expect_error(assert_pos(c(1.1, 2.2)))
  expect_error(assert_pos(numeric(0L)))
  expect_error(assert_pos(NA_real_))
  expect_error(assert_pos("1"))
})

test_that("brm_error()", {
  expect_error(brm_error("message"), class = "brm_error")
})

test_that("brm_warn()", {
  expect_warning(brm_warn("message"), class = "brm_warn")
})
