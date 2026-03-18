# Append simulated categorical covariates

Simulate and append non-time-varying categorical covariates to an
existing [`brm_data()`](brm_data.md) dataset.

## Usage

``` r
brm_simulate_categorical(data, names, levels, probabilities = NULL)
```

## Arguments

- data:

  Classed `tibble` as from [`brm_data()`](brm_data.md) or
  [`brm_simulate_outline()`](brm_simulate_outline.md).

- names:

  Character vector with the names of the new covariates to simulate and
  append. Names must all be unique and must not already be column names
  of `data`.

- levels:

  Character vector of unique levels of the simulated categorical
  covariates.

- probabilities:

  Either `NULL` or a numeric vector of length `length(levels)` with
  levels between 0 and 1 where all elements sum to 1. If `NULL`, then
  all levels are equally likely to be drawn. If not `NULL`, then
  `probabilities` is a vector of sampling probabilities corresponding to
  each respective level of `levels`.

## Value

A classed `tibble`, like from [`brm_data()`](brm_data.md) or
[`brm_simulate_outline()`](brm_simulate_outline.md), but with new
categorical covariate columns and with the names of the new covariates
appended to the `brm_covariates` attribute. Each new categorical
covariate column is a character vector, not the factor type in base R.

## Details

Each covariate is a new column of the dataset with one independent
random categorical draw for each patient, using a fixed set of levels
(via [`base::sample()`](https://rdrr.io/r/base/sample.html) with
`replace = TRUE`). All covariates simulated this way are independent of
everything else in the data, including other covariates (to the extent
that the random number generators in R work as intended).

## See also

Other simulation:
[`brm_simulate_continuous()`](brm_simulate_continuous.md),
[`brm_simulate_outline()`](brm_simulate_outline.md),
[`brm_simulate_prior()`](brm_simulate_prior.md),
[`brm_simulate_simple()`](brm_simulate_simple.md)

## Examples

``` r
data <- brm_simulate_outline()
brm_simulate_categorical(
  data = data,
  names = c("site", "region"),
  levels = c("area1", "area2")
)
#> # A tibble: 800 × 7
#>    patient     time   group   missing response site  region
#>    <chr>       <chr>  <chr>   <lgl>      <dbl> <chr> <chr> 
#>  1 patient_001 time_1 group_1 FALSE         NA area2 area2 
#>  2 patient_001 time_2 group_1 FALSE         NA area2 area2 
#>  3 patient_001 time_3 group_1 FALSE         NA area2 area2 
#>  4 patient_001 time_4 group_1 FALSE         NA area2 area2 
#>  5 patient_002 time_1 group_1 FALSE         NA area1 area2 
#>  6 patient_002 time_2 group_1 FALSE         NA area1 area2 
#>  7 patient_002 time_3 group_1 TRUE          NA area1 area2 
#>  8 patient_002 time_4 group_1 TRUE          NA area1 area2 
#>  9 patient_003 time_1 group_1 FALSE         NA area2 area2 
#> 10 patient_003 time_2 group_1 FALSE         NA area2 area2 
#> # ℹ 790 more rows
brm_simulate_categorical(
  data = data,
  names = c("site", "region"),
  levels = c("area1", "area2"),
  probabilities = c(0.1, 0.9)
)
#> # A tibble: 800 × 7
#>    patient     time   group   missing response site  region
#>    <chr>       <chr>  <chr>   <lgl>      <dbl> <chr> <chr> 
#>  1 patient_001 time_1 group_1 FALSE         NA area2 area2 
#>  2 patient_001 time_2 group_1 FALSE         NA area2 area2 
#>  3 patient_001 time_3 group_1 FALSE         NA area2 area2 
#>  4 patient_001 time_4 group_1 FALSE         NA area2 area2 
#>  5 patient_002 time_1 group_1 FALSE         NA area2 area2 
#>  6 patient_002 time_2 group_1 FALSE         NA area2 area2 
#>  7 patient_002 time_3 group_1 TRUE          NA area2 area2 
#>  8 patient_002 time_4 group_1 TRUE          NA area2 area2 
#>  9 patient_003 time_1 group_1 FALSE         NA area2 area2 
#> 10 patient_003 time_2 group_1 FALSE         NA area2 area2 
#> # ℹ 790 more rows
```
