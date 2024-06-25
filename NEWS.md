# brms.mmrm 1.0.1

* Handle outcome `NA`s in `get_draws_sigma()`.
* Improve `summary()` messages for informative prior archetypes.
* Rewrite the `archetypes.Rmd` vignette using the FEV dataset from the `mmrm` package.
* Add `brm_prior_template()`.

# brms.mmrm 1.0.0

## New features

* Add informative prior archetypes (#96, #101).
* Add [brm_formula_sigma()] to allow more flexibility for modeling standard deviations as distributional parameters (#102). Due to the complexities of computing marginal means of standard deviations in rare scenarios, [brm_marginal_draws()] does not return effect size if [brm_formula_sigma()] uses baseline or covariates.

## Guardrails to ensure the appropriateness of marginal mean estimation

* Require a new `formula` argument in `brm_marginal_draws()`.
* Change class name `"brm_data"` to `"brms_mmrm_data"` to align with other class names.
* Create a special `"brms_mmrm_formula"` class to wrap around the model formula. The class ensures that formulas passed to the model were created by `brms_formula()`, and the attributes store the user's choice of fixed effects.
* Create a special `"brms_mmrm_model"` class for fitted model objects. The class ensures that fitted models were created by `brms_model()`, and the attributes store the `"brms_mmrm_formula"` object in a way that `brms` itself cannot modify.
* Deprecate `use_subgroup` in `brm_marginal_draws()`. The subgroup is now always part of the reference grid when declared in `brm_data()`. To marginalize over subgroup, declare it in `covariates` instead.
* Prevent overplotting multiple subgroups in `brm_plot_compare()`.
* Update the subgroup vignette to reflect all the changes above.

## Custom estimation of marginal means

* Implement a new `brm_transform_marginal()` to transform model parameters to marginal means (#53).
* Use `brm_transform_marginal()` instead of `emmeans` in `brm_marginal_draws()` to derive posterior draws of marginal means based on posterior draws of model parameters (#53).
* Explain the custom marginal mean calculation in a new `inference.Rmd` vignette.
* Rename `methods.Rmd` to `model.Rmd` since `inference.Rmd` also discusses methods.

## Other improvements

* Extend `brm_formula()` and `brm_marginal_draws()` to optionally model homogeneous variances, as well as ARMA, AR, MA, and compound symmetry correlation structures.
* Restrict `brm_model()` to continuous families with identity links.
* In `brm_prior_simple()`, deprecate the `correlation` argument in favor of individual correlation-specific arguments such as `unstructured` and `compound_symmetry`.
* Ensure model matrices are full rank (#99).

# brms.mmrm 0.1.0

* Deprecate `brm_simulate()` in favor of `brm_simulate_simple()` (#3). The latter has a more specific name to disambiguate it from other simulation functions, and its parameterization conforms to the one in the methods vignette.
* Add new functions for nuanced simulations: `brm_simulate_outline()`, `brm_simulate_continuous()`, `brm_simulate_categorical()` (#3).
* In `brm_model()`, remove rows with missing responses. These rows are automatically removed by `brms` anyway, and by handling by handling this in `brms.mmrm`, we avoid a warning.
* Add subgroup analysis functionality and validate the subgroup model with simulation-based calibration (#18).
* Zero-pad numeric indexes in simulated data so the levels sort as expected.
* In `brm_data()`, deprecate `level_control` in favor of `reference_group`.
* In `brm_data()`, deprecate `level_baseline` in favor of `reference_time`.
* In `brm_formula()`, deprecate arguments `effect_baseline`, `effect_group`, `effect_time`, `interaction_baseline`, and `interaction_group` in favor of `baseline`, `group`, `time`, `baseline_time`, and `group_time`, respectively.
* Propagate values in the `missing` column in `brm_data_change()` such that a value in the change from baseline is labeled missing if either the baseline response is missing or the post-baseline response is missing.
* Change the names in the output of `brm_marginal_draws()` to be more internally consistent and fit better with the addition of subgroup-specific marginals (#18).
* Allow `brm_plot_compare()` and `brm_plot_draws()` to select the x axis variable and faceting variables.
* Allow `brm_plot_compare()` to choose the primary comparison of interest (source of the data, discrete time, treatment group, or subgroup level).

# brms.mmrm 0.0.2

* Fix grammatical issues in the description.

# brms.mmrm 0.0.1

* First version.
