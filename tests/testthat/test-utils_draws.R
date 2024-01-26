test_that("names_have_subgroup()", {
  expect_true(names_have_subgroup(c("x|y|z", "a|b|_c", "|a|b", "||")))
  expect_false(names_have_subgroup(c("xy|z", "a|b_c", "a|", "|")))
  expect_error(names_have_subgroup(c("a|b", "a||b")), class = "brm_error")
})
