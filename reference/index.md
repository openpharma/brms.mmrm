# Package index

## Help

- [`brms.mmrm-package`](brms.mmrm-package.md) :

  brms.mmrm: Bayesian MMRMs using `brms`

## Data

- [`brm_data()`](brm_data.md) : Create and preprocess an MMRM dataset.
- [`brm_data_change()`](brm_data_change.md) : Convert to change from
  baseline.
- [`brm_data_chronologize()`](brm_data_chronologize.md) : Chronologize a
  dataset

## Simulation

- [`brm_simulate_categorical()`](brm_simulate_categorical.md) : Append
  simulated categorical covariates
- [`brm_simulate_continuous()`](brm_simulate_continuous.md) : Append
  simulated continuous covariates
- [`brm_simulate_outline()`](brm_simulate_outline.md) : Start a
  simulated dataset
- [`brm_simulate_prior()`](brm_simulate_prior.md) : Prior predictive
  draws.
- [`brm_simulate_simple()`](brm_simulate_simple.md) : Simple MMRM
  simulation.

## Informative prior archetypes

- [`brm_archetype_average_cells()`](brm_archetype_average_cells.md) :
  Cell-means-like time-averaged archetype
- [`brm_archetype_average_effects()`](brm_archetype_average_effects.md)
  : Treatment effect time-averaged archetype
- [`brm_archetype_cells()`](brm_archetype_cells.md) : Cell means
  archetype
- [`brm_archetype_effects()`](brm_archetype_effects.md) : Treatment
  effect archetype
- [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md)
  : Cell-means-like successive differences archetype
- [`brm_archetype_successive_effects()`](brm_archetype_successive_effects.md)
  : Treatment-effect-like successive differences archetype

## Archetype utilities

- [`brm_recenter_nuisance()`](brm_recenter_nuisance.md) : Recenter
  nuisance variables

## Priors

- [`brm_prior_archetype()`](brm_prior_archetype.md) : Informative priors
  for fixed effects in archetypes

- [`brm_prior_label()`](brm_prior_label.md) : Label a prior with levels
  in the data.

- [`brm_prior_simple()`](brm_prior_simple.md) :

  Simple prior for a `brms` MMRM

- [`brm_prior_template()`](brm_prior_template.md) : Label template for
  informative prior archetypes

## Models

- [`brm_formula()`](brm_formula.md) : Model formula
- [`brm_formula_sigma()`](brm_formula_sigma.md) : Formula for standard
  deviation parameters
- [`brm_model()`](brm_model.md) : Fit an MMRM.

## Marginals

- [`brm_marginal_data()`](brm_marginal_data.md) : Marginal summaries of
  the data.
- [`brm_marginal_draws()`](brm_marginal_draws.md) : MCMC draws from the
  marginal posterior of an MMRM
- [`brm_marginal_draws_average()`](brm_marginal_draws_average.md) :
  Average marginal MCMC draws across time points.
- [`brm_marginal_grid()`](brm_marginal_grid.md) : Marginal names grid.
- [`brm_marginal_summaries()`](brm_marginal_summaries.md) : Summary
  statistics of the marginal posterior of an MMRM.
- [`brm_marginal_probabilities()`](brm_marginal_probabilities.md) :
  Marginal probabilities on the treatment effect for an MMRM.

## Transformations

- [`brm_transform_marginal()`](brm_transform_marginal.md) : Marginal
  mean transformation

## Visualization

- [`brm_plot_compare()`](brm_plot_compare.md) : Visually compare the
  marginals of multiple models and/or datasets.
- [`brm_plot_draws()`](brm_plot_draws.md) : Visualize posterior draws of
  marginals.
