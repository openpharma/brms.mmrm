# Simple MMRM simulation.

Simple function to simulate a dataset from a simple specialized MMRM.

## Usage

``` r
brm_simulate_simple(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  hyper_beta = 1,
  hyper_tau = 0.1,
  hyper_lambda = 1
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
  of the fixed effect parameters `beta`.

- hyper_tau:

  Positive numeric of length 1, hyperparameter. Prior standard deviation
  parameter of the residual log standard deviation parameters `tau`

- hyper_lambda:

  Positive numeric of length 1, hyperparameter. Prior shape parameter of
  the LKJ correlation matrix of the residuals among discrete time
  points.

## Value

A list of three objects:

- `data`: A tidy dataset with one row per patient per discrete time
  point and columns for the outcome and ID variables.

- `model_matrix`: A matrix with one row per row of `data` and columns
  that represent levels of the covariates.

- `parameters`: A named list of parameter draws sampled from the prior:

  - `beta`: numeric vector of fixed effects.

  - `tau`: numeric vector of residual log standard parameters for each
    time point.

  - `sigma`: numeric vector of residual standard parameters for each
    time point. `sigma` is equal to `exp(tau)`.

  - `lambda`: correlation matrix of the residuals among the time points
    within each patient.

  - `covariance`: covariance matrix of the residuals among the time
    points within each patient. `covariance` is equal to
    `diag(sigma) %*% lambda %*% diag(sigma)`.

## Details

Refer to the methods vignette for a full model specification. The
`brm_simulate_simple()` function simulates a dataset from a simple
pre-defined MMRM. It assumes a cell means structure for fixed effects,
which means there is one fixed effect scalar parameter (element of
vector `beta`) for each unique combination of levels of treatment group
and discrete time point. The elements of `beta` have independent
univariate normal priors with mean 0 and standard deviation
`hyper_beta`. The residual log standard deviation parameters (elements
of vector `tau`) have normal priors with mean 0 and standard deviation
`hyper_tau`. The residual correlation matrix parameter `lambda` has an
LKJ correlation prior with shape parameter `hyper_lambda`.

## See also

Other simulation:
[`brm_simulate_categorical()`](brm_simulate_categorical.md),
[`brm_simulate_continuous()`](brm_simulate_continuous.md),
[`brm_simulate_outline()`](brm_simulate_outline.md),
[`brm_simulate_prior()`](brm_simulate_prior.md)

## Examples

``` r
set.seed(0L)
simulation <- brm_simulate_simple()
simulation$data
#> # A tibble: 800 × 4
#>    patient     time   response group  
#>    <chr>       <chr>     <dbl> <chr>  
#>  1 patient_001 time_1    1.47  group_1
#>  2 patient_001 time_2    3.10  group_1
#>  3 patient_001 time_3    2.22  group_1
#>  4 patient_001 time_4    0.215 group_1
#>  5 patient_002 time_1    1.03  group_1
#>  6 patient_002 time_2    2.28  group_1
#>  7 patient_002 time_3    2.36  group_1
#>  8 patient_002 time_4    2.33  group_1
#>  9 patient_003 time_1    0.128 group_1
#> 10 patient_003 time_2    1.89  group_1
#> # ℹ 790 more rows
```
