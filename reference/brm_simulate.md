# Deprecated: simulate an MMRM.

Deprecated on 2023-09-01 (version 0.0.2.9001). Use
[`brm_simulate_simple()`](brm_simulate_simple.md) instead.

## Usage

``` r
brm_simulate(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  hyper_beta = 1,
  hyper_sigma = 1,
  hyper_correlation = 1
)
```

## Arguments

- n_group:

  Positive integer of length 1, number of treatment groups.

- n_patient:

  Positive integer of length 1, number of patients per treatment group.

- n_time:

  Positive integer of length 1, number of discrete time points (e.g.
  scheduled study visits) per patient.

- hyper_beta:

  Positive numeric of length 1, hyperparameter. Prior standard deviation
  of the fixed effect parameters.

- hyper_sigma:

  Positive numeric of length 1, hyperparameter. Uniform prior upper
  bound of the time-specific residual standard deviation parameters.

- hyper_correlation:

  Positive numeric of length 1, hyperparameter. LKJ shape parameter of
  the correlation matrix among repeated measures within each patient.

## Value

A list of three objects:

- `data`: A tidy dataset with one row per patient per discrete time
  point and columns for the response and covariates.

- `model_matrix`: A matrix with one row per row of `data` and columns
  that represent levels of the covariates.

- `parameters`: A named list of parameter values sampled from the prior.

## Examples

``` r
set.seed(0L)
simulation <- suppressWarnings(brm_simulate())
simulation$data
#> # A tibble: 800 × 4
#>    response group   patient   time  
#>       <dbl> <chr>   <chr>     <chr> 
#>  1     1.30 group 1 patient 1 time 1
#>  2     2.52 group 1 patient 1 time 2
#>  3     2.63 group 1 patient 1 time 3
#>  4     1.98 group 1 patient 1 time 4
#>  5     1.22 group 1 patient 2 time 1
#>  6     2.63 group 1 patient 2 time 2
#>  7     2.38 group 1 patient 2 time 3
#>  8     2.52 group 1 patient 2 time 4
#>  9     1.32 group 1 patient 3 time 1
#> 10     2.63 group 1 patient 3 time 2
#> # ℹ 790 more rows
```
