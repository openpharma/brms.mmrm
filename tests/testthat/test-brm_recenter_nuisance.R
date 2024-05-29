test_that("multiplication works", {
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 4,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_data_change() |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  archetype <- brm_archetype_cells(data)
  expect_equal(mean(archetype$nuisance_biomarker1), 0)
  expect_equal(
    mean(data$biomarker1),
    attr(archetype$nuisance_biomarker1, "brm_center")
  )
  expect_equal(
    max(abs((data$biomarker1 - center) - archetype$nuisance_biomarker1)),
    0
  )
  archetype <- brm_recenter_nuisance(
    data = archetype,
    nuisance = "nuisance_biomarker1",
    center = 0.75
  )
  expect_equal(attr(archetype$nuisance_biomarker1, "brm_center"), 0.75)
  expect_equal(
    max(abs((data$biomarker1 - 0.75) - archetype$nuisance_biomarker1)),
    0
  )
})
