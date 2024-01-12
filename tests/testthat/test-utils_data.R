test_that("unname_df()", {
  x <- unname_df(tibble::tibble(x = c(a = 1, b = 2), y = c(c = 3, d = 4)))
  expect_null(names(x$x))
  expect_null(names(x$y))
})

test_that("zero_pad_integers", {
  expect_equal(
    zero_pad_integers(c(1L, 0L, 2L, 5L, 7L)),
    c("1", "0", "2", "5", "7")
  )
  expect_equal(
    zero_pad_integers(c(1L, 10L, 0L, 2L, 5L, 7L)),
    c("01", "10", "00", "02", "05", "07")
  )
  expect_equal(
    zero_pad_integers(c(1L, 10L, 0L, 207L, 2L, 5L, 7L)),
    c("001", "010", "000", "207", "002", "005", "007")
  )
})
