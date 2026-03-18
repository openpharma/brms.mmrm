# Marginal mean transformation

Transformation from model parameters to marginal means.

## Usage

``` r
brm_transform_marginal(
  data,
  formula,
  average_within_subgroup = NULL,
  prefix = "b_"
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

- average_within_subgroup:

  `TRUE` to average concomitant covariates proportionally within
  subgroup levels, `FALSE` to average these covariates across the whole
  dataset. If `average_within_subgroup` is `NULL` (default), and if the
  model has a subgroup and nuisance variables, then
  `brm_transform_marginal()` prints and informative message (once per
  session) and sets `average_within_subgroup` to `FALSE`. If you see
  this message, please read
  <https://openpharma.github.io/brms.mmrm/articles/inference.html>,
  decide whether to set `average_within_subgroup` to `TRUE` or `FALSE`
  in `brm_transform_marginal()`, and then manually supply the output of
  `brm_transform_marginal()` to the `transform` argument of
  [`brm_marginal_draws()`](brm_marginal_draws.md).

  To create marginal means, `brms.mmrm` conditions the nuisance
  covariates on their averages across the whole dataset
  (`average_within_subgroup = FALSE` or `NULL` in
  `brm_transform_marginal()`). This may be reasonable in some cases, and
  it mitigates the kind of hidden confounding between the subgroup and
  other variables which may otherwise cause Simpson's paradox. However,
  for subgroup-specific marginal means, it may not be realistic to
  condition on a single point estimate for all levels of the reference
  grid (for example, if the subgroup is female vs male, but all marginal
  means condition on a single overall observed pregnancy rate of 5%). In
  these situations, it may be appropriate to instead condition on
  subgroup-specific averages of nuisance variables
  (`average_within_subgroup = TRUE` in `brm_transform_marginal()`). But
  if you do this, it is your responsibility to investigate and
  understand the hidden interactions and confounding in your dataset.
  For more information, please visit
  <https://openpharma.github.io/brms.mmrm/articles/inference.html> and
  <https://cran.r-project.org/package=emmeans/vignettes/interactions.html>.

- prefix:

  Character of length 1, prefix to add to the model matrix (`"X"`) from
  [`brms::make_standata()`](https://paulbuerkner.com/brms/reference/standata.html)
  in order to reconstruct the `brms` model parameter names. This
  argument should only be modified for testing purposes.

## Value

A matrix to transform model parameters (columns) into marginal means
(rows).

## Details

The matrix from `brm_transform_marginal()` is passed to the
`transform_marginal` argument of
[`brm_marginal_draws()`](brm_marginal_draws.md), and it transforms
posterior draws of model parameters to posterior draws of marginal
means. You may customize the output of `brm_transform_marginal()` before
passing it to [`brm_marginal_draws()`](brm_marginal_draws.md). However,
please do not modify the dimensions, row names, or column names.

## Examples

``` r
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
transform <- brm_transform_marginal(data = data, formula = formula)
equations <- summary(transform)
#> # This is a matrix to transform model parameters to marginal means.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and fixed effect parameters
#> # (right-hand side).
#> # 
#> #   group_1:time_1 = b_Intercept
#> #   group_1:time_2 = b_Intercept + b_timetime_2
#> #   group_1:time_3 = b_Intercept + b_timetime_3
#> #   group_1:time_4 = b_Intercept + b_timetime_4
#> #   group_2:time_1 = b_Intercept + b_groupgroup_2
#> #   group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2
#> #   group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3
#> #   group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4
print(equations)
#> [1] "group_1:time_1 = b_Intercept"                                                            
#> [2] "group_1:time_2 = b_Intercept + b_timetime_2"                                             
#> [3] "group_1:time_3 = b_Intercept + b_timetime_3"                                             
#> [4] "group_1:time_4 = b_Intercept + b_timetime_4"                                             
#> [5] "group_2:time_1 = b_Intercept + b_groupgroup_2"                                           
#> [6] "group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2"
#> [7] "group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3"
#> [8] "group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4"
summary(transform, message = FALSE)
#> [1] "group_1:time_1 = b_Intercept"                                                            
#> [2] "group_1:time_2 = b_Intercept + b_timetime_2"                                             
#> [3] "group_1:time_3 = b_Intercept + b_timetime_3"                                             
#> [4] "group_1:time_4 = b_Intercept + b_timetime_4"                                             
#> [5] "group_2:time_1 = b_Intercept + b_groupgroup_2"                                           
#> [6] "group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2"
#> [7] "group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3"
#> [8] "group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4"
class(transform)
#> [1] "brms_mmrm_transform_marginal" "matrix"                      
#> [3] "array"                       
print(transform)
#>                b_Intercept b_groupgroup_2 b_timetime_2 b_timetime_3
#> group_1|time_1           1              0            0            0
#> group_1|time_2           1              0            1            0
#> group_1|time_3           1              0            0            1
#> group_1|time_4           1              0            0            0
#> group_2|time_1           1              1            0            0
#> group_2|time_2           1              1            1            0
#> group_2|time_3           1              1            0            1
#> group_2|time_4           1              1            0            0
#>                b_timetime_4 b_groupgroup_2:timetime_2 b_groupgroup_2:timetime_3
#> group_1|time_1            0                         0                         0
#> group_1|time_2            0                         0                         0
#> group_1|time_3            0                         0                         0
#> group_1|time_4            1                         0                         0
#> group_2|time_1            0                         0                         0
#> group_2|time_2            0                         1                         0
#> group_2|time_3            0                         0                         1
#> group_2|time_4            1                         0                         0
#>                b_groupgroup_2:timetime_4
#> group_1|time_1                         0
#> group_1|time_2                         0
#> group_1|time_3                         0
#> group_1|time_4                         0
#> group_2|time_1                         0
#> group_2|time_2                         0
#> group_2|time_3                         0
#> group_2|time_4                         1
```
