# Start a simulated dataset

Begin creating a simulated dataset.

## Usage

``` r
brm_simulate_outline(
  n_group = 2L,
  n_subgroup = NULL,
  n_patient = 100L,
  n_time = 4L,
  rate_dropout = 0.1,
  rate_lapse = 0.05
)
```

## Arguments

- n_group:

  Positive integer of length 1, number of treatment groups.

- n_subgroup:

  Positive integer of length 1, number of subgroup levels. Set to `NULL`
  to omit the subgroup entirely.

- n_patient:

  Positive integer of length 1. If `n_subgroup` is `NULL`, then
  `n_patient` is the number of patients per treatment group. Otherwise,
  `n_patient` is the number of patients per treatment group *per
  subgroup*. In both cases, the total number of patients in the whole
  simulated dataset is usually much greater than the `n_patients`
  argument of `brm_simulate_outline()`.

- n_time:

  Positive integer of length 1, number of discrete time points (e.g.
  scheduled study visits) per patient.

- rate_dropout:

  Numeric of length 1 between 0 and 1, post-baseline dropout rate. A
  dropout is an intercurrent event when data collection for a patient
  stops permanently, causing the outcomes for that patient to be missing
  during and after the dropout occurred. The first time point is assumed
  to be baseline, so dropout is there. Dropouts are equally likely to
  occur at each of the post-baseline time points.

- rate_lapse:

  Numeric of length 1, expected proportion of post-baseline outcomes
  that are missing. Missing outcomes of this type are independent and
  uniformly distributed across the data.

## Value

A classed data frame from [`brm_data()`](brm_data.md). The data frame
has one row per patient per time point and the following columns:

- `group`: integer index of the treatment group.

- `patient`: integer index of the patient.

- `time`: integer index of the discrete time point.

## See also

Other simulation:
[`brm_simulate_categorical()`](brm_simulate_categorical.md),
[`brm_simulate_continuous()`](brm_simulate_continuous.md),
[`brm_simulate_prior()`](brm_simulate_prior.md),
[`brm_simulate_simple()`](brm_simulate_simple.md)

## Examples

``` r
brm_simulate_outline()
#> # A tibble: 800 × 5
#>    patient     time   group   missing response
#>    <chr>       <chr>  <chr>   <lgl>      <dbl>
#>  1 patient_001 time_1 group_1 FALSE         NA
#>  2 patient_001 time_2 group_1 FALSE         NA
#>  3 patient_001 time_3 group_1 FALSE         NA
#>  4 patient_001 time_4 group_1 FALSE         NA
#>  5 patient_002 time_1 group_1 FALSE         NA
#>  6 patient_002 time_2 group_1 FALSE         NA
#>  7 patient_002 time_3 group_1 FALSE         NA
#>  8 patient_002 time_4 group_1 FALSE         NA
#>  9 patient_003 time_1 group_1 FALSE         NA
#> 10 patient_003 time_2 group_1 FALSE         NA
#> # ℹ 790 more rows
```
