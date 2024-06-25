test_that("brm_prior_template()", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  archetype <- brm_archetype_successive_cells(data)
  label <- brm_prior_template(archetype)
  expect_equal(dim(label), c(6L, 3L))
  expect_equal(label$code, rep(NA_character_, 6L))
  expect_equal(label$group, rep(c("group_1", "group_2"), each = 3L))
  expect_equal(label$time, rep(paste0("time_", seq_len(3L)), times = 2L))
  label$code <- c(
    "normal(1, 1)",
    "normal(1, 2)",
    "normal(1, 3)",
    "normal(2, 1)",
    "normal(2, 2)",
    "normal(2, 3)"
  )
  prior <- brm_prior_archetype(label = label, archetype = archetype)
  expect_equal(nrow(prior), 6L)
  expect_s3_class(prior, "brmsprior")
  expect_equal(prior$class, rep("b", 6L))
  expect_equal(
    sort(prior$coef),
    sort(
      paste0(
        "x_group_",
        rep(c(1L, 2L), each = 3L),
        "_time_", rep(seq_len(3L), times = 2L)
      )
    )
  )
})
