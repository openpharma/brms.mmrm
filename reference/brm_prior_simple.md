# Simple prior for a `brms` MMRM

Generate a simple prior for a `brms` MMRM.

## Usage

``` r
brm_prior_simple(
  data,
  formula,
  intercept = "student_t(3, 0, 2.5)",
  coefficients = "student_t(3, 0, 2.5)",
  sigma = "student_t(3, 0, 2.5)",
  unstructured = "lkj(1)",
  autoregressive = "",
  moving_average = "",
  compound_symmetry = "",
  correlation = NULL
)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- formula:

  An object of class `"brmsformula"` from
  [`brm_formula()`](brm_formula.md) or
  [`brms::brmsformula()`](https://paulbuerkner.com/brms/reference/brmsformula.html).
  Should include the full mapping of the model, including fixed effects,
  residual correlation, and heterogeneity in the discrete-time-specific
  residual variance components.

- intercept:

  Character of length 1, Stan code for the prior to set on the intercept
  parameter.

- coefficients:

  Character of length 1, Stan code for the prior to set independently on
  each of the non-intercept model coefficients.

- sigma:

  Character of length 1, Stan code for the prior to set independently on
  each of the log-scale standard deviation parameters. Should be a
  symmetric prior in most situations.

- unstructured:

  Character of length 1, Stan code for an unstructured correlation
  prior. Supply the empty string `""` to set a flat prior (default).
  Applies to the `"cortime` parameter class in `brms` priors. Used for
  formulas created with `brm_formula(correlation = "unstructured")`. LKJ
  is recommended. See also
  [`brms::unstr()`](https://paulbuerkner.com/brms/reference/unstr.html).

- autoregressive:

  Character of length 1, Stan code for a prior on autoregressive
  correlation parameters. Supply the empty string `""` to set a flat
  prior (default). Applies to the `"ar` parameter class in `brms`
  priors. Used for formulas created with
  `brm_formula(correlation = "autoregressive")` and
  `brm_formula(correlation = "autoregressive_moving_average")`. See also
  `brms::ar()` and
  [`brms::arma()`](https://paulbuerkner.com/brms/reference/arma.html).

- moving_average:

  Character of length 1, Stan code for a prior on moving average
  correlation parameters. Supply the empty string `""` to set a flat
  prior (default). Applies to the `"ma` parameter class in `brms`
  priors. Used for formulas created with
  `brm_formula(correlation = "moving_average")` and
  `brm_formula(correlation = "autoregressive_moving_average")`. See also
  [`brms::ma()`](https://paulbuerkner.com/brms/reference/ma.html) and
  [`brms::arma()`](https://paulbuerkner.com/brms/reference/arma.html).

- compound_symmetry:

  Character of length 1, Stan code for a prior on compound symmetry
  correlation parameters. Supply the empty string `""` to set a flat
  prior (default). Applies to the `"cosy` parameter class in `brms`
  priors. Used for formulas created with
  `brm_formula(correlation = "compound_symmetry")`. See also
  [`brms::cosy()`](https://paulbuerkner.com/brms/reference/cosy.html).

- correlation:

  Deprecated on 2024-04-22 (version 0.1.0.9004). Please use arguments
  like `"unstructured"`, and/or `"autoregressive"` to supply
  correlation-specific priors.

## Value

A classed data frame with the `brms` prior.

## Details

In `brm_prior_simple()`, you can separately choose priors for the
intercept, model coefficients, log-scale standard deviations, and
pairwise correlations between time points within patients. However, each
class of parameters is set as a whole. In other words,
`brm_prior_simple()` cannot assign different priors to different fixed
effect parameters.

## See also

Other priors: [`brm_prior_archetype()`](brm_prior_archetype.md),
[`brm_prior_label()`](brm_prior_label.md),
[`brm_prior_template()`](brm_prior_template.md)

## Examples

``` r
set.seed(0L)
data <- brm_simulate_outline()
data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
formula <- brm_formula(
  data = data,
  baseline = FALSE,
  baseline_time = FALSE,
  check_rank = FALSE
)
brm_prior_simple(
  data = data,
  formula = formula,
  intercept = "student_t(3, 0, 2.5)",
  coefficients = "normal(0, 10)",
  sigma = "student_t(2, 0, 4)",
  unstructured = "lkj(2.5)"
)
#>                 prior     class coef group resp  dpar nlpar lb ub tag  source
#>  student_t(3, 0, 2.5) Intercept                                       default
#>         normal(0, 10)         b                                       default
#>              lkj(2.5)   cortime                                       default
#>    student_t(2, 0, 4)         b                 sigma                 default
```
