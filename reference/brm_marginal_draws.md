# MCMC draws from the marginal posterior of an MMRM

Get marginal posterior draws from a fitted MMRM.

## Usage

``` r
brm_marginal_draws(
  model,
  data = model$brms.mmrm_data,
  formula = model$brms.mmrm_formula,
  transform = brms.mmrm::brm_transform_marginal(data = data, formula = formula,
    average_within_subgroup = average_within_subgroup),
  effect_size = attr(formula, "brm_allow_effect_size"),
  average_within_subgroup = NULL,
  use_subgroup = NULL,
  control = NULL,
  baseline = NULL
)
```

## Arguments

- model:

  A fitted model object from [`brm_model()`](brm_model.md).

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

- transform:

  Matrix with one row per marginal mean and one column per model
  parameter. `brm_marginal_draws()` uses this matrix to map posterior
  draws of model parameters to posterior draws of marginal means using
  matrix multiplication. Please use
  [`brm_transform_marginal()`](brm_transform_marginal.md) to compute
  this matrix and then modify only if necessary. See the methods
  vignettes for details on this matrix, as well as how `brms.mmrm`
  computes marginal means more generally.

- effect_size:

  Logical, `TRUE` to derive posterior samples of effect size (treatment
  effect divided by residual standard deviation). `FALSE` to omit.
  `brms.mmrm` does not support effect size when baseline or covariates
  are included in the [`brm_formula_sigma()`](brm_formula_sigma.md)
  formula. If `effect_size` is `TRUE` in this case, then
  `brm_marginal_draws()` will automatically omit effect size and throw
  an informative warning.

- average_within_subgroup:

  `TRUE`, `FALSE`, or `NULL` to control whether nuisance parameters are
  averaged within subgroup levels in
  [`brm_transform_marginal()`](brm_transform_marginal.md). Ignored if
  the `transform` argument is manually supplied by the user. See the
  help page of [`brm_transform_marginal()`](brm_transform_marginal.md)
  for details on the `average_within_subgroup` argument.

- use_subgroup:

  Deprecated. No longer used. `brm_marginal_draws()` no longer
  marginalizes over the subgroup declared in
  [`brm_data()`](brm_data.md). To marginalize over the subgroup, declare
  that variable in `covariates` instead.

- control:

  Deprecated. Set the control group level in
  [`brm_data()`](brm_data.md).

- baseline:

  Deprecated. Set the control group level in
  [`brm_data()`](brm_data.md).

## Value

A named list of tibbles of MCMC draws of the marginal posterior
distribution of each treatment group and time point. These marginals are
also subgroup-specific if [`brm_formula()`](brm_formula.md) included
fixed effects that use the `subgroup` variable originally declared in
[`brm_data()`](brm_data.md). In each tibble, there is 1 row per
posterior sample and one column for each type of marginal distribution
(i.e. each combination of treatment group and discrete time point. The
specific `tibble`s in the returned list are described below:

- `response`: on the scale of the response variable.

- `difference_time`: change from baseline: the `response` at a
  particular time minus the `response` at baseline (`reference_time`).
  Only returned if the `reference_time` argument of
  [`brm_data()`](brm_data.md) was not `NULL` (i.e. if a baseline value
  for the time variable was identified).

- `difference_group`: treatment effect: These samples depend on the
  values of `reference_group` and `reference_time` which were originally
  declared in [`brm_data()`](brm_data.md). `reference_group` is the
  control group, and `reference_time` is baseline. If baseline was
  originally given (via `reference_time` in
  [`brm_data()`](brm_data.md)), then `difference_time` is the
  change-from-baseline value of each active group minus that of the
  control group. Otherwise, if baseline is omitted (i.e.
  `reference_time = NULL` (default) in [`brm_data()`](brm_data.md)),
  then `difference_time` is the raw response at each active group minus
  that of the control group.

- `difference_subgroup`: subgroup differences: the `difference_group` at
  each subgroup level minus the `difference_group` at the subgroup
  reference level (`reference_subgroup`). Only reported if a subgroup
  analysis was specified through the appropriate arguments to
  [`brm_data()`](brm_data.md) and [`brm_formula()`](brm_formula.md).

- `effect`: effect size, defined as the treatment difference divided by
  the residual standard deviation. Omitted if the `effect_size` argument
  is `FALSE` or if the [`brm_formula_sigma()`](brm_formula_sigma.md)
  includes baseline or covariates.

- `sigma`: posterior draws of linear-scale marginal standard deviations
  of residuals. Omitted if the `effect_size` argument is `FALSE` or if
  the [`brm_formula_sigma()`](brm_formula_sigma.md) includes baseline or
  covariates.

## Baseline

The returned values from `brm_marginal_draws()` depend on whether a
baseline time point was declared through the `reference_time` argument
of [`brm_data()`](brm_data.md). If `reference_time` was not `NULL`, then
`brm_marginal_draws()` will calculate change from baseline, and it will
calculate treatment differences as differences between
change-from-baseline values. If `reference_time` was not `NULL`, then
`brm_marginal_draws()` will not calculate change from baseline, and it
will calculate treatment differences as differences between response
values.

## Separation string

Post-processing in `brm_marginal_draws()` names each of the
group-by-time marginal means with the delimiting character string from
`Sys.getenv("BRM_SEP", unset = "|")`. Neither the column names nor
element names of the group and time variables can contain this string.
To set a custom string yourself, use
`Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")`.

## See also

Other marginals: [`brm_marginal_data()`](brm_marginal_data.md),
[`brm_marginal_draws_average()`](brm_marginal_draws_average.md),
[`brm_marginal_grid()`](brm_marginal_grid.md),
[`brm_marginal_probabilities()`](brm_marginal_probabilities.md),
[`brm_marginal_summaries()`](brm_marginal_summaries.md)

## Examples

``` r
if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
set.seed(0L)
data <- brm_data(
  data = brm_simulate_simple()$data,
  outcome = "response",
  group = "group",
  time = "time",
  patient = "patient",
  reference_group = "group_1",
  reference_time = "time_1"
)
formula <- brm_formula(
  data = data,
  baseline = FALSE,
  baseline_time = FALSE
)
tmp <- utils::capture.output(
  suppressMessages(
    suppressWarnings(
      model <- brm_model(
        data = data,
        formula = formula,
        chains = 1,
        iter = 100,
        refresh = 0
      )
    )
  )
)
brm_marginal_draws(data = data, formula = formula, model = model)
}
```
