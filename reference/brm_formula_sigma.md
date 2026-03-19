# Formula for standard deviation parameters

Parameterize standard deviations using a formula for the `sigma`
argument of [`brm_formula()`](brm_formula.md).

## Usage

``` r
brm_formula_sigma(
  data,
  check_rank = TRUE,
  intercept = FALSE,
  baseline = FALSE,
  baseline_subgroup = FALSE,
  baseline_subgroup_time = FALSE,
  baseline_time = FALSE,
  covariates = FALSE,
  group = FALSE,
  group_subgroup = FALSE,
  group_subgroup_time = FALSE,
  group_time = FALSE,
  subgroup = FALSE,
  subgroup_time = FALSE,
  time = TRUE
)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- check_rank:

  `TRUE` to check the rank of the model matrix for `sigma` and throw an
  error if rank deficiency is detected. `FALSE` to skip this check.
  Rank-deficiency may cause `sigma` to be non-identifiable, may prevent
  the MCMC from converging.

- intercept:

  Logical of length 1. `TRUE` (default) to include an intercept, `FALSE`
  to omit.

- baseline:

  Logical of length 1. `TRUE` to include an additive effect for baseline
  response, `FALSE` to omit. If `TRUE`, then effect size will be omitted
  from the output of [`brm_marginal_draws()`](brm_marginal_draws.md).

- baseline_subgroup:

  Logical of length 1.

- baseline_subgroup_time:

  Logical of length 1. `TRUE` to include baseline-by-subgroup-by-time
  interaction, `FALSE` to omit. If `TRUE`, then effect size will be
  omitted from the output of
  [`brm_marginal_draws()`](brm_marginal_draws.md).

- baseline_time:

  Logical of length 1. `TRUE` to include baseline-by-time interaction,
  `FALSE` to omit. If `TRUE`, then effect size will be omitted from the
  output of [`brm_marginal_draws()`](brm_marginal_draws.md).

- covariates:

  Logical of length 1. `TRUE` (default) to include any additive
  covariates declared with the `covariates` argument of
  [`brm_data()`](brm_data.md), `FALSE` to omit. If `TRUE`, then effect
  size will be omitted from the output of
  [`brm_marginal_draws()`](brm_marginal_draws.md).

- group:

  Logical of length 1. `TRUE` (default) to include additive effects for
  treatment groups, `FALSE` to omit.

- group_subgroup:

  Logical of length 1. `TRUE` to include group-by-subgroup interaction,
  `FALSE` to omit.

- group_subgroup_time:

  Logical of length 1. `TRUE` to include group-by-subgroup-by-time
  interaction, `FALSE` to omit.

- group_time:

  Logical of length 1.

- subgroup:

  Logical of length 1. `TRUE` to include additive fixed effects for
  subgroup levels, `FALSE` to omit.

- subgroup_time:

  Logical of length 1. `TRUE` to include subgroup-by-time interaction,
  `FALSE` to omit.

- time:

  Logical of length 1.

## Value

A base R formula with S3 class `"brms_mmrm_formula_sigma"`. This formula
controls the parameterization of `sigma`, the linear-scale `brms`
distributional parameters which represent standard deviations.

## Details

In `brms`, the standard deviations of the residuals are modeled through
a parameter vector called `sigma`. `brms.mmrm` always treats `sigma` as
a distributional parameter
(<https://paulbuerkner.com/brms/articles/brms_distreg.html>).
`brm_formula_sigma()` lets you control the parameterization of `sigma`.
The output of `brm_formula_sigma()` serves as input to the `sigma`
argument of [`brm_formula()`](brm_formula.md).

The default `sigma` formula is `sigma ~ 0 + time`, where `time` is the
discrete time variable in the data. This is the usual heterogeneous
variance structure which declares one standard deviation parameter for
each time point in the data. Alternatively, you could write
`brm_formula_sigma(data, intercept = TRUE, time = FALSE)`. This will
produce `sigma ~ 1`, which yields a single scalar variance (a structure
termed "homogeneous variance").

With arguments like `baseline` and `covariates`, you can specify
extremely complicated variance structures. However, if baseline or
covariates are used, then the output of
[`brm_marginal_draws()`](brm_marginal_draws.md) omit effect size due to
the statistical challenges of calculating marginal means of draws of
`sigma` for this uncommon scenario.

## See also

Other models: [`brm_formula()`](brm_formula.md),
[`brm_model()`](brm_model.md)

## Examples

``` r
set.seed(0)
data <- brm_data(
  data = brm_simulate_simple()$data,
  outcome = "response",
  group = "group",
  time = "time",
  patient = "patient",
  reference_group = "group_1",
  reference_time = "time_1"
)
homogeneous <- brm_formula_sigma(data, time = FALSE, intercept = TRUE)
by_group <- brm_formula_sigma(data, group = TRUE, intercept = TRUE)
homogeneous
#> sigma ~ 1
#> attr(,"brm_allow_effect_size")
#> [1] TRUE
#> <environment: 0x555b8e0b69c0>
by_group
#> sigma ~ group + time
#> attr(,"brm_allow_effect_size")
#> [1] TRUE
#> <environment: 0x555b92b4dbb8>
brm_formula(data, sigma = homogeneous)
#> response ~ group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ 1
brm_formula(data, sigma = by_group)
#> response ~ group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ group + time
```
