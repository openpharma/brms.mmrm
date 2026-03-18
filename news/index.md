# Changelog

## brms.mmrm 1.1.1.9000 (development)

## brms.mmrm 1.1.1

CRAN release: 2024-10-02

- Use FEV data in usage vignette.
- Show how to visualize prior vs posterior in the usage vignette.
- Add a `center` argument to `brms_formula.default()` and explain
  intercept parameter interpretation concerns
  ([\#128](https://github.com/openpharma/brms.mmrm/issues/128)).

## brms.mmrm 1.1.0

CRAN release: 2024-07-29

- Add [`brm_marginal_grid()`](../reference/brm_marginal_grid.md).
- Show posterior samples of `sigma` in
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md) and
  [`brm_marginal_summaries()`](../reference/brm_marginal_summaries.md).
- Allow `outcome = "response"` with `reference_time = NULL`. Sometimes
  raw response is analyzed but the data has no baseline time point.
- Preserve factors in [`brm_data()`](../reference/brm_data.md) and
  encourage ordered factors for the time variable
  ([\#113](https://github.com/openpharma/brms.mmrm/issues/113)).
- Add [`brm_data_chronologize()`](../reference/brm_data_chronologize.md)
  to ensure the correctness of the time variable.
- Do not drop columns in [`brm_data()`](../reference/brm_data.md). This
  helps
  [`brm_data_chronologize()`](../reference/brm_data_chronologize.md)
  operate correctly after calls to
  [`brm_data()`](../reference/brm_data.md).
- Add new elements `brms.mmrm_data` and `brms.mmrm_formula` to the
  `brms` fitted model object returned by
  [`brm_model()`](../reference/brm_model.md).
- Take defaults `data` and `formula` from the above in
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md).
- Set the default value of `effect_size` to
  `attr(formula, "brm_allow_effect_size")`.
- Remove defaults from some arguments to
  [`brm_data()`](../reference/brm_data.md) and document examples.
- Deprecate the `role` argument of
  [`brm_data()`](../reference/brm_data.md) in favor of `reference_time`
  ([\#119](https://github.com/openpharma/brms.mmrm/issues/119)).
- Add a new `model_missing_outcomes` in
  [`brm_formula()`](../reference/brm_formula.md) to optionally impute
  missing values during model fitting as described at
  <https://paulbuerkner.com/brms/articles/brms_missings.html>
  ([\#121](https://github.com/openpharma/brms.mmrm/issues/121)).
- Add a new `imputed` argument to accept a `mice` multiply imputed
  dataset (“mids”) in [`brm_model()`](../reference/brm_model.md)
  ([\#121](https://github.com/openpharma/brms.mmrm/issues/121)).
- Add a [`summary()`](https://rdrr.io/r/base/summary.html) method for
  [`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
  objects.
- Do not recheck the rank of the formula in
  [`brm_transform_marginal()`](../reference/brm_transform_marginal.md).
- Support constrained longitudinal data analysis (cLDA) for informative
  prior archetypes
  [`brm_archetype_cells()`](../reference/brm_archetype_cells.md),
  [`brm_archetype_effects()`](../reference/brm_archetype_effects.md),
  [`brm_archetype_successive_cells()`](../reference/brm_archetype_successive_cells.md),
  and
  [`brm_archetype_successive_effects()`](../reference/brm_archetype_successive_effects.md)
  ([\#125](https://github.com/openpharma/brms.mmrm/issues/125)). We
  cannot support cLDA for
  [`brm_archetype_average_cells()`](../reference/brm_archetype_average_cells.md)
  or
  [`brm_archetype_average_effects()`](../reference/brm_archetype_average_effects.md)
  because then some parameters would no longer be averages of others.

## brms.mmrm 1.0.1

CRAN release: 2024-06-25

- Handle outcome `NA`s in `get_draws_sigma()`.
- Improve [`summary()`](https://rdrr.io/r/base/summary.html) messages
  for informative prior archetypes.
- Rewrite the `archetypes.Rmd` vignette using the FEV dataset from the
  `mmrm` package.
- Add [`brm_prior_template()`](../reference/brm_prior_template.md).

## brms.mmrm 1.0.0

CRAN release: 2024-06-04

### New features

- Add informative prior archetypes
  ([\#96](https://github.com/openpharma/brms.mmrm/issues/96),
  [\#101](https://github.com/openpharma/brms.mmrm/issues/101)).
- Add \[brm_formula_sigma()\] to allow more flexibility for modeling
  standard deviations as distributional parameters
  ([\#102](https://github.com/openpharma/brms.mmrm/issues/102)). Due to
  the complexities of computing marginal means of standard deviations in
  rare scenarios, \[brm_marginal_draws()\] does not return effect size
  if \[brm_formula_sigma()\] uses baseline or covariates.

### Guardrails to ensure the appropriateness of marginal mean estimation

- Require a new `formula` argument in
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md).
- Change class name `"brm_data"` to `"brms_mmrm_data"` to align with
  other class names.
- Create a special `"brms_mmrm_formula"` class to wrap around the model
  formula. The class ensures that formulas passed to the model were
  created by `brms_formula()`, and the attributes store the user’s
  choice of fixed effects.
- Create a special `"brms_mmrm_model"` class for fitted model objects.
  The class ensures that fitted models were created by `brms_model()`,
  and the attributes store the `"brms_mmrm_formula"` object in a way
  that `brms` itself cannot modify.
- Deprecate `use_subgroup` in
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md). The
  subgroup is now always part of the reference grid when declared in
  [`brm_data()`](../reference/brm_data.md). To marginalize over
  subgroup, declare it in `covariates` instead.
- Prevent overplotting multiple subgroups in
  [`brm_plot_compare()`](../reference/brm_plot_compare.md).
- Update the subgroup vignette to reflect all the changes above.

### Custom estimation of marginal means

- Implement a new
  [`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
  to transform model parameters to marginal means
  ([\#53](https://github.com/openpharma/brms.mmrm/issues/53)).
- Use
  [`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
  instead of `emmeans` in
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md) to derive
  posterior draws of marginal means based on posterior draws of model
  parameters
  ([\#53](https://github.com/openpharma/brms.mmrm/issues/53)).
- Explain the custom marginal mean calculation in a new `inference.Rmd`
  vignette.
- Rename `methods.Rmd` to `model.Rmd` since `inference.Rmd` also
  discusses methods.

### Other improvements

- Extend [`brm_formula()`](../reference/brm_formula.md) and
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md) to
  optionally model homogeneous variances, as well as ARMA, AR, MA, and
  compound symmetry correlation structures.
- Restrict [`brm_model()`](../reference/brm_model.md) to continuous
  families with identity links.
- In [`brm_prior_simple()`](../reference/brm_prior_simple.md), deprecate
  the `correlation` argument in favor of individual correlation-specific
  arguments such as `unstructured` and `compound_symmetry`.
- Ensure model matrices are full rank
  ([\#99](https://github.com/openpharma/brms.mmrm/issues/99)).

## brms.mmrm 0.1.0

CRAN release: 2024-02-15

- Deprecate [`brm_simulate()`](../reference/brm_simulate.md) in favor of
  [`brm_simulate_simple()`](../reference/brm_simulate_simple.md)
  ([\#3](https://github.com/openpharma/brms.mmrm/issues/3)). The latter
  has a more specific name to disambiguate it from other simulation
  functions, and its parameterization conforms to the one in the methods
  vignette.
- Add new functions for nuanced simulations:
  [`brm_simulate_outline()`](../reference/brm_simulate_outline.md),
  [`brm_simulate_continuous()`](../reference/brm_simulate_continuous.md),
  [`brm_simulate_categorical()`](../reference/brm_simulate_categorical.md)
  ([\#3](https://github.com/openpharma/brms.mmrm/issues/3)).
- In [`brm_model()`](../reference/brm_model.md), remove rows with
  missing responses. These rows are automatically removed by `brms`
  anyway, and by handling by handling this in `brms.mmrm`, we avoid a
  warning.
- Add subgroup analysis functionality and validate the subgroup model
  with simulation-based calibration
  ([\#18](https://github.com/openpharma/brms.mmrm/issues/18)).
- Zero-pad numeric indexes in simulated data so the levels sort as
  expected.
- In [`brm_data()`](../reference/brm_data.md), deprecate `level_control`
  in favor of `reference_group`.
- In [`brm_data()`](../reference/brm_data.md), deprecate
  `level_baseline` in favor of `reference_time`.
- In [`brm_formula()`](../reference/brm_formula.md), deprecate arguments
  `effect_baseline`, `effect_group`, `effect_time`,
  `interaction_baseline`, and `interaction_group` in favor of
  `baseline`, `group`, `time`, `baseline_time`, and `group_time`,
  respectively.
- Propagate values in the `missing` column in
  [`brm_data_change()`](../reference/brm_data_change.md) such that a
  value in the change from baseline is labeled missing if either the
  baseline response is missing or the post-baseline response is missing.
- Change the names in the output of
  [`brm_marginal_draws()`](../reference/brm_marginal_draws.md) to be
  more internally consistent and fit better with the addition of
  subgroup-specific marginals
  ([\#18](https://github.com/openpharma/brms.mmrm/issues/18)).
- Allow [`brm_plot_compare()`](../reference/brm_plot_compare.md) and
  [`brm_plot_draws()`](../reference/brm_plot_draws.md) to select the x
  axis variable and faceting variables.
- Allow [`brm_plot_compare()`](../reference/brm_plot_compare.md) to
  choose the primary comparison of interest (source of the data,
  discrete time, treatment group, or subgroup level).

## brms.mmrm 0.0.2

CRAN release: 2023-08-18

- Fix grammatical issues in the description.

## brms.mmrm 0.0.1

- First version.
