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
  center <- mean(data$biomarker1)
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

test_that("brm_recenter_nuisance() recenters within the cell", {
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
    brm_simulate_continuous(names = c("biomarker1", "biomarker2"))
  archetype <- brm_archetype_cells(data, baseline_time = TRUE)
  nuisance <- "nuisance_baseline.timetime_2"
  mask <- attr(archetype[[nuisance]], "brm_mask")
  expect_equal(mean(archetype[[nuisance]][mask]), 0)
  # Recentering at zero restores raw baseline in the cell the column
  # applies to, and it leaves the rest of the column at zero.
  archetype <- brm_recenter_nuisance(
    data = archetype,
    nuisance = nuisance,
    center = 0
  )
  expect_equal(attr(archetype[[nuisance]], "brm_center"), 0)
  expect_equal(max(abs(archetype[[nuisance]][mask] - data$baseline[mask])), 0)
  expect_true(all(archetype[[nuisance]][!mask] == 0))
})
