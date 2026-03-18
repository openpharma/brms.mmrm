# Treatment effect time-averaged archetype

Create a treatment effect informative prior archetype with a special
fixed effect to represent the average across time.

## Usage

``` r
brm_archetype_average_effects(
  data,
  intercept = FALSE,
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) && !is.null(attr(data,
    "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) && !is.null(attr(data,
    "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline")),
  covariates = TRUE,
  prefix_interest = "x_",
  prefix_nuisance = "nuisance_"
)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

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
  rather than in [`brm_formula()`](brm_formula.md) in order to make sure
  columns are appropriately centered and the underlying model matrix has
  full rank.

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
  rather than in [`brm_formula()`](brm_formula.md) in order to make sure
  columns are appropriately centered and the underlying model matrix has
  full rank.

- baseline_time:

  Logical of length 1. `TRUE` to include baseline-by-time interaction,
  `FALSE` to omit. Default is `TRUE` if [`brm_data()`](brm_data.md)
  previously declared a baseline variable in the dataset. Ignored for
  informative prior archetypes. For informative prior archetypes, this
  option should be set in functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in [`brm_formula()`](brm_formula.md) in order to make sure
  columns are appropriately centered and the underlying model matrix has
  full rank.

- covariates:

  Logical of length 1. `TRUE` (default) to include any additive
  covariates declared with the `covariates` argument of
  [`brm_data()`](brm_data.md), `FALSE` to omit. For informative prior
  archetypes, this option is set in functions like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  rather than in [`brm_formula()`](brm_formula.md) in order to make sure
  columns are appropriately centered and the underlying model matrix has
  full rank.

- prefix_interest:

  Character string to prepend to the new columns of generated fixed
  effects of interest (relating to group, subgroup, and/or time). In
  rare cases, you may need to set a non-default prefix to prevent name
  conflicts with existing columns in the data, or rename the columns in
  your data. `prefix_interest` must not be the same value as
  `prefix_nuisance`.

- prefix_nuisance:

  Same as `prefix_interest`, but relating to generated fixed effects NOT
  of interest (not relating to group, subgroup, or time). Must not be
  the same value as `prefix_interest`.

## Value

A special classed `tibble` with data tailored to the treatment effect
time-averaged archetype. The dataset is augmented with extra columns
with the `"archetype_"` prefix, as well as special attributes to tell
downstream functions like [`brm_formula()`](brm_formula.md) what to do
with the object.

## Details

This archetype has a special fixed effect for each treatment group to
represent the mean response averaged across all the time points, and
treatment effects are explicitly parameterized.

To illustrate, suppose the dataset has two treatment groups A
(placebo/reference group) and B (active/non-reference group), time
points 1, 2, and 3, and no other covariates. Let `mu_gt` be the marginal
mean of the response at group `g` time `t` given data and
hyperparameters. The model has fixed effect parameters `beta_1`,
`beta_2`, ..., `beta_6` which express the marginal means `mu_gt` as
follows:

    `mu_A1 = 3 * beta_1 - beta_2 - beta_3`
    `mu_A2 = beta_2`
    `mu_A3 = beta_3`

    `mu_B1 = 3 * beta_1 - beta_2 - beta_3 + 3 * beta_4 - beta_5 - beta_6`
    `mu_B2 = beta_2 + beta_5`
    `mu_B3 = beta_3 + beta_6`

For group A, `beta_1` is the average response in group A averaged across
time points. You can confirm this yourself by expressing the average
across time `(mu_A1 + mu_A2 + mu_A3) / 3` in terms of the `beta_*`
parameters and confirming that the expression simplifies down to just
`beta_1`. `beta_2` represents the mean response in group A at time 2,
and `beta_3` represents the mean response in group A at time 3. `beta_4`
is the treatment effect of group B relative to group A, averaged across
time points. `beta_5` is the treatment effect of B vs A at time 2, and
`beta_6` is analogous for time 3.

## Prior labeling for `brm_archetype_average_effects()`

Within each treatment group, the initial time point represents the
average, and each successive time point represents the response within
that actual time. To illustrate, consider the example in the Details
section. In the labeling scheme for `brm_archetype_average_effects()`,
you can label the prior on `beta_1` using
`brm_prior_label(code = "normal(1.2, 5)", group = "A", time = "1")`.
Similarly, you cal label the prior on `beta_5` with
`brm_prior_label(code = "normal(1.3, 7)", group = "B", time = "2")`. To
confirm that you set the prior correctly, compare the `brms` prior with
the output of `summary(your_archetype)`. See the examples for details.

## Nuisance variables

In the presence of covariate adjustment, functions like
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
convert nuisance factors into binary dummy variables, then center all
those dummy variables and any continuous nuisance variables at their
means in the data. This ensures that the main model coefficients of
interest are not implicitly conditional on a subset of the data. In
other words, preprocessing nuisance variables this way preserves the
interpretations of the fixed effects of interest, and it ensures
informative priors can be specified correctly.

## Prior labeling

Informative prior archetypes use a labeling scheme to assign priors to
fixed effects. How it works:

    1. First, assign the prior of each parameter a collection
      of labels from the data. This can be done manually or with
      successive calls to [brm_prior_label()].
    2. Supply the labeling scheme to [brm_prior_archetype()].
      [brm_prior_archetype()] uses attributes of the archetype
      to map labeled priors to their rightful parameters in the model.

For informative prior archetypes, this process is much more convenient
and robust than manually calling
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
However, it requires an understanding of how the labels of the priors
map to parameters in the model. This mapping varies from archetype to
archetype, and it is documented in the help pages of archetype-specific
functions such as
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

## See also

Other informative prior archetypes:
[`brm_archetype_average_cells()`](brm_archetype_average_cells.md),
[`brm_archetype_cells()`](brm_archetype_cells.md),
[`brm_archetype_effects()`](brm_archetype_effects.md),
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md),
[`brm_archetype_successive_effects()`](brm_archetype_successive_effects.md)

## Examples

``` r
set.seed(0L)
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
    names = c("status1", "status2"),
    levels = c("present", "absent")
  )
dplyr::select(
  data,
  group,
  time,
  patient,
  starts_with("biomarker"),
  starts_with("status")
)
#> # A tibble: 600 × 7
#>    group   time   patient     biomarker1 biomarker2 status1 status2
#>    <chr>   <chr>  <chr>            <dbl>      <dbl> <chr>   <chr>  
#>  1 group_1 time_2 patient_001     -1.42      -0.287 absent  present
#>  2 group_1 time_3 patient_001     -1.42      -0.287 absent  present
#>  3 group_1 time_4 patient_001     -1.42      -0.287 absent  present
#>  4 group_1 time_2 patient_002     -1.67       1.84  absent  present
#>  5 group_1 time_3 patient_002     -1.67       1.84  absent  present
#>  6 group_1 time_4 patient_002     -1.67       1.84  absent  present
#>  7 group_1 time_2 patient_003      1.38      -0.157 absent  absent 
#>  8 group_1 time_3 patient_003      1.38      -0.157 absent  absent 
#>  9 group_1 time_4 patient_003      1.38      -0.157 absent  absent 
#> 10 group_1 time_2 patient_004     -0.920     -1.39  present present
#> # ℹ 590 more rows
archetype <- brm_archetype_average_effects(data)
archetype
#> # A tibble: 600 × 23
#>    x_group_1_time_2 x_group_1_time_3 x_group_1_time_4 x_group_2_time_2
#>  *            <int>            <int>            <int>            <int>
#>  1                3               -1               -1                0
#>  2                0                1                0                0
#>  3                0                0                1                0
#>  4                3               -1               -1                0
#>  5                0                1                0                0
#>  6                0                0                1                0
#>  7                3               -1               -1                0
#>  8                0                1                0                0
#>  9                0                0                1                0
#> 10                3               -1               -1                0
#> # ℹ 590 more rows
#> # ℹ 19 more variables: x_group_2_time_3 <int>, x_group_2_time_4 <int>,
#> #   nuisance_biomarker1 <dbl>, nuisance_biomarker2 <dbl>,
#> #   nuisance_status1_absent <dbl>, nuisance_status2_present <dbl>,
#> #   nuisance_baseline <dbl>, nuisance_baseline.timetime_2 <dbl>,
#> #   nuisance_baseline.timetime_3 <dbl>, patient <chr>, time <chr>, group <chr>,
#> #   missing <lgl>, change <dbl>, baseline <dbl>, biomarker1 <dbl>, …
summary(archetype)
#> # This is the "average effects" informative prior archetype in brms.mmrm.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and important fixed effect parameters
#> # (right-hand side). Nuisance parameters are omitted.
#> # 
#> #   group_1:time_2 = 3*x_group_1_time_2 - x_group_1_time_3 - x_group_1_time_4
#> #   group_1:time_3 = x_group_1_time_3
#> #   group_1:time_4 = x_group_1_time_4
#> #   group_2:time_2 = 3*x_group_1_time_2 - x_group_1_time_3 - x_group_1_time_4 + 3*x_group_2_time_2 - x_group_2_time_3 - x_group_2_time_4
#> #   group_2:time_3 = x_group_1_time_3 + x_group_2_time_3
#> #   group_2:time_4 = x_group_1_time_4 + x_group_2_time_4
formula <- brm_formula(archetype)
formula
#> change ~ 0 + x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4 + x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4 + nuisance_biomarker1 + nuisance_biomarker2 + nuisance_status1_absent + nuisance_status2_present + nuisance_baseline + nuisance_baseline.timetime_2 + nuisance_baseline.timetime_3 + unstr(time = time, gr = patient) 
#> sigma ~ 0 + time
prior <- brm_prior_label(
  code = "normal(1, 2.2)",
  group = "group_1",
  time = "time_2"
) |>
  brm_prior_label("normal(1, 3.3)", group = "group_1", time = "time_3") |>
  brm_prior_label("normal(1, 4.4)", group = "group_1", time = "time_4") |>
  brm_prior_label("normal(2, 2.2)", group = "group_2", time = "time_2") |>
  brm_prior_label("normal(2, 3.3)", group = "group_2", time = "time_3") |>
  brm_prior_label("normal(2, 4.4)", group = "group_2", time = "time_4") |>
  brm_prior_archetype(archetype)
prior
#>           prior class             coef group resp dpar nlpar   lb   ub tag
#>  normal(1, 2.2)     b x_group_1_time_2                       <NA> <NA>    
#>  normal(1, 3.3)     b x_group_1_time_3                       <NA> <NA>    
#>  normal(1, 4.4)     b x_group_1_time_4                       <NA> <NA>    
#>  normal(2, 2.2)     b x_group_2_time_2                       <NA> <NA>    
#>  normal(2, 3.3)     b x_group_2_time_3                       <NA> <NA>    
#>  normal(2, 4.4)     b x_group_2_time_4                       <NA> <NA>    
#>  source
#>    user
#>    user
#>    user
#>    user
#>    user
#>    user
class(prior)
#> [1] "brmsprior"  "data.frame"
if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
tmp <- utils::capture.output(
  suppressMessages(
    suppressWarnings(
      model <- brm_model(
        data = archetype,
        formula = formula,
        prior = prior,
        chains = 1,
        iter = 100,
        refresh = 0
      )
    )
  )
)
suppressWarnings(print(model))
brms::prior_summary(model)
draws <- brm_marginal_draws(
  data = archetype,
  formula = formula,
  model = model
)
summaries_model <- brm_marginal_summaries(draws)
summaries_data <- brm_marginal_data(data)
brm_plot_compare(model = summaries_model, data = summaries_data)
}
```
