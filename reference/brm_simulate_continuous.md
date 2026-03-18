# Append simulated continuous covariates

Simulate and append non-time-varying continuous covariates to an
existing [`brm_data()`](brm_data.md) dataset.

## Usage

``` r
brm_simulate_continuous(data, names, mean = 0, sd = 1)
```

## Arguments

- data:

  Classed `tibble` as from [`brm_data()`](brm_data.md) or
  [`brm_simulate_outline()`](brm_simulate_outline.md).

- names:

  Character vector with the names of the new covariates to simulate and
  append. Names must all be unique and must not already be column names
  of `data`.

- mean:

  Numeric of length 1, mean of the normal distribution for simulating
  each covariate.

- sd:

  Positive numeric of length 1, standard deviation of the normal
  distribution for simulating each covariate.

## Value

A classed `tibble`, like from [`brm_data()`](brm_data.md) or
[`brm_simulate_outline()`](brm_simulate_outline.md), but with new
numeric covariate columns and with the names of the new covariates
appended to the `brm_covariates` attribute.

## Details

Each covariate is a new column of the dataset with one independent
random univariate normal draw for each patient. All covariates simulated
this way are independent of everything else in the data, including other
covariates (to the extent that the random number generators in R work as
intended).

## See also

Other simulation:
[`brm_simulate_categorical()`](brm_simulate_categorical.md),
[`brm_simulate_outline()`](brm_simulate_outline.md),
[`brm_simulate_prior()`](brm_simulate_prior.md),
[`brm_simulate_simple()`](brm_simulate_simple.md)

## Examples

``` r
data <- brm_simulate_outline()
brm_simulate_continuous(
  data = data,
  names = c("age", "biomarker")
)
#> # A tibble: 800 × 7
#>    patient     time   group   missing response     age biomarker
#>    <chr>       <chr>  <chr>   <lgl>      <dbl>   <dbl>     <dbl>
#>  1 patient_001 time_1 group_1 FALSE         NA -0.0499    -1.30 
#>  2 patient_001 time_2 group_1 FALSE         NA -0.0499    -1.30 
#>  3 patient_001 time_3 group_1 FALSE         NA -0.0499    -1.30 
#>  4 patient_001 time_4 group_1 FALSE         NA -0.0499    -1.30 
#>  5 patient_002 time_1 group_1 FALSE         NA  1.75       0.592
#>  6 patient_002 time_2 group_1 FALSE         NA  1.75       0.592
#>  7 patient_002 time_3 group_1 FALSE         NA  1.75       0.592
#>  8 patient_002 time_4 group_1 FALSE         NA  1.75       0.592
#>  9 patient_003 time_1 group_1 FALSE         NA  0.400      1.05 
#> 10 patient_003 time_2 group_1 FALSE         NA  0.400      1.05 
#> # ℹ 790 more rows
brm_simulate_continuous(
  data = data,
  names = c("biomarker1", "biomarker2"),
  mean = 1000,
  sd = 100
)
#> # A tibble: 800 × 7
#>    patient     time   group   missing response biomarker1 biomarker2
#>    <chr>       <chr>  <chr>   <lgl>      <dbl>      <dbl>      <dbl>
#>  1 patient_001 time_1 group_1 FALSE         NA      1154.      1066.
#>  2 patient_001 time_2 group_1 FALSE         NA      1154.      1066.
#>  3 patient_001 time_3 group_1 FALSE         NA      1154.      1066.
#>  4 patient_001 time_4 group_1 FALSE         NA      1154.      1066.
#>  5 patient_002 time_1 group_1 FALSE         NA      1108.      1084.
#>  6 patient_002 time_2 group_1 FALSE         NA      1108.      1084.
#>  7 patient_002 time_3 group_1 FALSE         NA      1108.      1084.
#>  8 patient_002 time_4 group_1 FALSE         NA      1108.      1084.
#>  9 patient_003 time_1 group_1 FALSE         NA       714.      1111.
#> 10 patient_003 time_2 group_1 FALSE         NA       714.      1111.
#> # ℹ 790 more rows
```
