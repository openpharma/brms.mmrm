# Model formula

Build a model formula for an MMRM, either for a generic
[`brm_data()`](brm_data.md) dataset or an informative prior archetype.

## Usage

``` r
brm_formula(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  ...
)

# Default S3 method
brm_formula(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  intercept = TRUE,
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) && !is.null(attr(data,
    "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) && !is.null(attr(data,
    "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline")),
  covariates = TRUE,
  group = TRUE,
  group_subgroup = !is.null(attr(data, "brm_subgroup")),
  group_subgroup_time = !is.null(attr(data, "brm_subgroup")),
  group_time = TRUE,
  subgroup = !is.null(attr(data, "brm_subgroup")),
  subgroup_time = !is.null(attr(data, "brm_subgroup")),
  time = TRUE,
  center = TRUE,
  ...,
  effect_baseline = NULL,
  effect_group = NULL,
  effect_time = NULL,
  interaction_baseline = NULL,
  interaction_group = NULL
)

# S3 method for class 'brms_mmrm_archetype'
brm_formula(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  ...,
  warn_ignored = TRUE
)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- model_missing_outcomes:

  Logical of length 1, `TRUE` to impute missing outcomes during model
  fitting as described in the "Imputation during model fitting" section
  of <https://paulbuerkner.com/brms/articles/brms_missings.html>.
  Specifically, if the outcome variable is `y`, then the formula will
  begin with `y | mi() ~ ...` instead of simply `y ~ ...`. Set to
  `FALSE` (default) to forgo this kind of imputation and discard missing
  observations from the data just prior to fitting the model inside
  [`brm_model()`](brm_model.md). See
  <https://opensource.nibr.com/bamdd/src/02h_mmrm.html#what-estimand-does-mmrm-address>
  \#nolint to understand the standard assumptions and decisions
  regarding MMRMs and missing outcomes.

- check_rank:

  `TRUE` to check the rank of the model matrix and throw an error if
  rank deficiency is detected. `FALSE` to skip this check.
  Rank-deficient models may have non-identifiable parameters and it is
  recommended to choose a full-rank mapping.

- sigma:

  A formula produced by [`brm_formula_sigma()`](brm_formula_sigma.md).
  The formula is a base R formula with S3 class
  `"brms_mmrm_formula_sigma"`, and it controls the parameterization of
  the residual standard deviations `sigma`.

- correlation:

  Character of length 1, name of the correlation structure. The
  correlation matrix is a square `T x T` matrix, where `T` is the number
  of discrete time points in the data. This matrix describes the
  correlations between time points in the same patient, as modeled in
  the residuals. Different patients are modeled as independent. The
  `correlation` argument controls how this matrix is parameterized, and
  the choices given by `brms` are listed at
  <https://paulbuerkner.com/brms/reference/autocor-terms.html>, and the
  choice is ultimately encoded in the main body of the output formula
  through terms like `unstru()` and `arma()`, some of which are
  configurable through arguments `autoregressive_order`,
  `moving_average_order`, and `residual_covariance_arma_estimation` of
  `brm_formula()`. Choices in `brms.mmrm`:

  - `"unstructured"`: the default/recommended option, a fully
    parameterized covariance matrix with a unique scalar parameter for
    each unique pair of discrete time points. C.f.
    <https://paulbuerkner.com/brms/reference/unstr.html>.

  - `"autoregressive_moving_average"`: autoregressive moving average
    (ARMA), c.f. <https://paulbuerkner.com/brms/reference/arma.html>.

  - `"autoregressive"`: autoregressive (AR), c.f.
    <https://paulbuerkner.com/brms/reference/ar.html>.

  - `"moving_average"`: moving average (MA), c.f.
    <https://paulbuerkner.com/brms/reference/ma.html>.

  - `"compound_symmetry`: compound symmetry, c.f.
    <https://paulbuerkner.com/brms/reference/cosy.html>.

  - `"diagonal"`: declare independent time points within patients.

- autoregressive_order:

  Nonnegative integer, autoregressive order for the
  `"autoregressive_moving_average"` and `"autoregressive"` correlation
  structures.

- moving_average_order:

  Nonnegative integer, moving average order for the
  `"autoregressive_moving_average"` and `"moving_average"` correlation
  structures.

- residual_covariance_arma_estimation:

  `TRUE` or `FALSE`, whether to estimate ARMA effects using residual
  covariance matrices. Directly supplied to the `cov` argument in `brms`
  for `"autoregressive_moving_average"`, `"autoregressive"`, and
  `"moving_average"` correlation structures. C.f.
  <https://paulbuerkner.com/brms/reference/arma.html>.

- ...:

  Named arguments to specific `brm_formula()` methods.

- intercept:

  Logical of length 1. `TRUE` (default) to include an intercept, `FALSE`
  to omit.

- baseline:

  Logical of length 1. `TRUE` to include an additive effect for baseline
  response, `FALSE` to omit. Default is `TRUE` if
  [`brm_data()`](brm_data.md) previously declared a baseline variable in
  the dataset. Ignored for informative prior archetypes. For informative
  prior archetypes, this option should be set in functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in `brm_formula()` in order to make sure columns are
  appropriately centered and the underlying model matrix has full rank.

- baseline_subgroup:

  Logical of length 1.

- baseline_subgroup_time:

  Logical of length 1. `TRUE` to include baseline-by-subgroup-by-time
  interaction, `FALSE` to omit. Default is `TRUE` if
  [`brm_data()`](brm_data.md) previously declared baseline and subgroup
  variables in the dataset. Ignored for informative prior archetypes.
  For informative prior archetypes, this option should be set in
  functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in `brm_formula()` in order to make sure columns are
  appropriately centered and the underlying model matrix has full rank.

- baseline_time:

  Logical of length 1. `TRUE` to include baseline-by-time interaction,
  `FALSE` to omit. Default is `TRUE` if [`brm_data()`](brm_data.md)
  previously declared a baseline variable in the dataset. Ignored for
  informative prior archetypes. For informative prior archetypes, this
  option should be set in functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in `brm_formula()` in order to make sure columns are
  appropriately centered and the underlying model matrix has full rank.

- covariates:

  Logical of length 1. `TRUE` (default) to include any additive
  covariates declared with the `covariates` argument of
  [`brm_data()`](brm_data.md), `FALSE` to omit. For informative prior
  archetypes, this option is set in functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in `brm_formula()` in order to make sure columns are
  appropriately centered and the underlying model matrix has full rank.

- group:

  Logical of length 1. `TRUE` (default) to include additive effects for
  treatment groups, `FALSE` to omit.

- group_subgroup:

  Logical of length 1. `TRUE` to include group-by-subgroup interaction,
  `FALSE` to omit. Default is `TRUE` if [`brm_data()`](brm_data.md)
  previously declared a subgroup variable in the dataset.

- group_subgroup_time:

  Logical of length 1. `TRUE` to include group-by-subgroup-by-time
  interaction, `FALSE` to omit. Default is `TRUE` if
  [`brm_data()`](brm_data.md) previously declared a subgroup variable in
  the dataset.

- group_time:

  Logical of length 1. `TRUE` (default) to include group-by-time
  interaction, `FALSE` to omit.

- subgroup:

  Logical of length 1. `TRUE` to include additive fixed effects for
  subgroup levels, `FALSE` to omit. Default is `TRUE` if
  [`brm_data()`](brm_data.md) previously declared a subgroup variable in
  the dataset.

- subgroup_time:

  Logical of length 1. `TRUE` to include subgroup-by-time interaction,
  `FALSE` to omit. Default is `TRUE` if [`brm_data()`](brm_data.md)
  previously declared a subgroup variable in the dataset.

- time:

  Logical of length 1. `TRUE` (default) to include a additive effect for
  discrete time, `FALSE` to omit.

- center:

  `TRUE` to center the columns of the model matrix before fitting the
  model if the model formula includes an intercept term controlled by
  `brms`. `FALSE` to skip centering. Centering usually leads to more
  computationally efficient sampling in the presence of an intercept,
  but it changes the interpretation of the intercept parameter if
  included in the model (as explained in the help file of
  [`brms::brmsformula()`](https://paulbuerkner.com/brms/reference/brmsformula.html)).
  Informative prior archetypes always use `center = FALSE` and use an
  intercept not controlled by `brms.mmrm` to ensure the intercept
  parameter is interpretable and compatible with user-defined priors.

- effect_baseline:

  Deprecated on 2024-01-16 (version 0.0.2.9002). Use `baseline` instead.

- effect_group:

  Deprecated on 2024-01-16 (version 0.0.2.9002). Use `group` instead.

- effect_time:

  Deprecated on 2024-01-16 (version 0.0.2.9002). Use `time` instead.

- interaction_baseline:

  Deprecated on 2024-01-16 (version 0.0.2.9002). Use `baseline_time`
  instead.

- interaction_group:

  Deprecated on 2024-01-16 (version 0.0.2.9002). Use `group_time`
  instead.

- warn_ignored:

  Set to `TRUE` to throw a warning if ignored arguments are specified,
  `FALSE` otherwise.

## Value

An object of class `"brmsformula"` returned from
[`brms::brmsformula()`](https://paulbuerkner.com/brms/reference/brmsformula.html).
It contains the fixed effect mapping, correlation structure, and
residual variance structure.

## [`brm_data()`](brm_data.md) formulas

For a [`brm_data()`](brm_data.md) dataset, `brm_formula()` builds an R
formula for an MMRM based on the details in the data and your choice of
mapping. Customize your mapping by toggling on or off the various
`TRUE`/`FALSE` arguments of `brm_formula()`, such as `intercept`,
`baseline`, and `group_time`. All plausible additive effects, two-way
interactions, and three-way interactions can be specified. The following
interactions are not supported:

- Any interactions with the concomitant covariates you specified in the
  `covariates` argument of [`brm_data()`](brm_data.md).

- Any interactions which include baseline response and treatment group
  together. Rationale: in a randomized controlled experiment, baseline
  and treatment group assignment should be uncorrelated.

## Formulas for informative prior archetypes

Functions like
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
tailor datasets to informative prior archetypes. For these specialized
tailored datasets, `brm_formula()` works differently. It still applies
the variance and correlation structure of your choosing, and it still
lets you choose whether to adjust for nuisance covariates, but it no
longer lets you toggle on/off individual terms in the model, such as
`intercept`, `baseline`, or `group`. Instead, to ensure the correct
interpretation of the parameters, `brm_formula()` uses the `x_*` and
`nuisance_*` columns generated by
`brm_archetype_successive_cells( prefix_interest = "x_", prefix_nuisance = "nuisance_")`.

## Parameterization

For a formula on a [`brm_data()`](brm_data.md) dataset, the formula is
not the only factor that determines the fixed effect mapping. The
ordering of the categorical variables in the data, as well as the
`contrast` option in R, affect the construction of the model matrix. To
see the model matrix that will ultimately be used in
[`brm_model()`](brm_model.md), run
[`brms::make_standata()`](https://paulbuerkner.com/brms/reference/standata.html)
and examine the `X` element of the returned list. See the examples below
for a demonstration.

## See also

Other models: [`brm_formula_sigma()`](brm_formula_sigma.md),
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
brm_formula(data)
#> response ~ group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ 0 + time
brm_formula(data = data, intercept = FALSE, baseline = FALSE)
#> response ~ 0 + group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ 0 + time
formula <- brm_formula(
  data = data,
  intercept = FALSE,
  baseline = FALSE,
  group = FALSE
)
formula
#> response ~ 0 + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ 0 + time
# Standard deviations of residuals are distributional parameters that can
# regress on variables in the data.
homogeneous <- brm_formula_sigma(data, time = FALSE)
by_group <- brm_formula_sigma(data, group = TRUE, intercept = TRUE)
homogeneous
#> sigma ~ 0
#> attr(,"brm_allow_effect_size")
#> [1] TRUE
#> <environment: 0x555b911e6650>
by_group
#> sigma ~ group + time
#> attr(,"brm_allow_effect_size")
#> [1] TRUE
#> <environment: 0x555b915187f8>
brm_formula(data, sigma = homogeneous)
#> response ~ group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ 0
brm_formula(data, sigma = by_group)
#> response ~ group + group:time + time + unstr(time = time, gr = patient) 
#> sigma ~ group + time
# Optional: set the contrast option, which determines the model matrix.
options(contrasts = c(unordered = "contr.SAS", ordered = "contr.poly"))
# See the fixed effect mapping you get from the data:
head(brms::make_standata(formula = formula, data = data)$X)
#>   timetime_1 timetime_2 timetime_3 timetime_4 timetime_1:groupgroup_1
#> 1          1          0          0          0                       1
#> 2          0          1          0          0                       0
#> 3          0          0          1          0                       0
#> 4          0          0          0          1                       0
#> 5          1          0          0          0                       1
#> 6          0          1          0          0                       0
#>   timetime_2:groupgroup_1 timetime_3:groupgroup_1 timetime_4:groupgroup_1
#> 1                       0                       0                       0
#> 2                       1                       0                       0
#> 3                       0                       1                       0
#> 4                       0                       0                       1
#> 5                       0                       0                       0
#> 6                       1                       0                       0
# Specify a different contrast method to use an alternative
# mapping when fitting the model with brm_model():
options(
  contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")
)
# different model matrix than before:
head(brms::make_standata(formula = formula, data = data)$X)
#>   timetime_1 timetime_2 timetime_3 timetime_4 timetime_1:groupgroup_2
#> 1          1          0          0          0                       0
#> 2          0          1          0          0                       0
#> 3          0          0          1          0                       0
#> 4          0          0          0          1                       0
#> 5          1          0          0          0                       0
#> 6          0          1          0          0                       0
#>   timetime_2:groupgroup_2 timetime_3:groupgroup_2 timetime_4:groupgroup_2
#> 1                       0                       0                       0
#> 2                       0                       0                       0
#> 3                       0                       0                       0
#> 4                       0                       0                       0
#> 5                       0                       0                       0
#> 6                       0                       0                       0
# Formula on an informative prior archetype:
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
    names = "biomarker3",
    levels = c("present", "absent")
  )
archetype <- brm_archetype_successive_cells(data)
formula <- brm_formula(data = archetype)
formula
#> change ~ 0 + x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4 + x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4 + nuisance_biomarker1 + nuisance_biomarker2 + nuisance_biomarker3_absent + nuisance_baseline + nuisance_baseline.timetime_2 + nuisance_baseline.timetime_3 + unstr(time = time, gr = patient) 
#> sigma ~ 0 + time
```
