# Recenter nuisance variables

Change the center of a nuisance variable of an informative prior
archetype.

## Usage

``` r
brm_recenter_nuisance(data, nuisance, center)
```

## Arguments

- data:

  An informative prior archetype data frame output from
  [`brm_archetype_cells()`](https://openpharma.github.io/brms.mmrm/reference/brm_archetype_cells.md)
  or similar.

- nuisance:

  Character of length 1, name of the nuisance column in the data to
  shift the center.

- center:

  Numeric of length 1, value of the center to shift the column in
  `nuisance`. The affected column in the returned archetype data frame
  will look as if it were centered by this value to begin with.

## Value

An informative prior archetype data frame with one of the variables
re-centered.

## Details

By "centering vector y at scalar x", we mean taking the difference
`z = y - x`. If `x` is the mean, then `mean(z)` is 0. Informative prior
archetypes center nuisance variables at their means so the parameters
can be interpreted correctly for setting informative priors. This is
appropriate most of the time, but sometimes it is better to center a
column at a pre-specified scientifically meaningful fixed number. If you
want a nuisance column to be centered at a fixed value other than its
mean, use `brm_recenter_nuisance()` to shift the center. This function
can handle any nuisance variable

For a nuisance variable which interacts baseline response with discrete
time and/or the subgroup, the center applies to baseline within the cell
of the data that the interaction applies to. The rest of the column
stays at zero.

Recentering a nuisance variable shifts the reference level of the model,
which changes the interpretation of the marginal means from
[`brm_marginal_draws()`](https://openpharma.github.io/brms.mmrm/reference/brm_marginal_draws.md).
It is your responsibility to interpret the reference level and the
resulting marginal means appropriately. Please recenter with
`brm_recenter_nuisance()` rather than modifying the column directly, so
the recorded center stays in sync with the column.
[`brm_transform_marginal()`](https://openpharma.github.io/brms.mmrm/reference/brm_transform_marginal.md)
prints an informative message (once per session) if it detects a
nuisance column that was recentered some other way.

## Examples

``` r
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
mean(archetype$nuisance_biomarker1) # after original centering
#> [1] -2.088553e-17
center <- mean(data$biomarker1)
center # original center, before the centering from brm_archetype_cells()
#> [1] -0.05507499
attr(archetype$nuisance_biomarker1, "brm_center") # original center
#> [1] -0.05507499
max(abs((data$biomarker1 - center) - archetype$nuisance_biomarker1))
#> [1] 0
# Re-center nuisance_biomarker1 at 0.75.
archetype <- brm_recenter_nuisance(
  data = archetype,
  nuisance = "nuisance_biomarker1",
  center = 0.75
)
attr(archetype$nuisance_biomarker1, "brm_center") # new center
#> [1] 0.75
mean(archetype$nuisance_biomarker1) # no longer equal to the center
#> [1] -0.805075
# nuisance_biomarker1 is now as though we centered it at 0.75.
max(abs((data$biomarker1 - 0.75) - archetype$nuisance_biomarker1))
#> [1] 2.220446e-16
```
