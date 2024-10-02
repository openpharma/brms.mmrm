# brms.mmrm 1.1.1

* Use FEV data in usage vignette.
* Show how to visualize prior vs posterior in the usage vignette.
* Add a `center` argument to `brms_formula.default()` and explain intercept parameter interpretation concerns (#128).

# brms.mmrm 1.1.0

* Add `brm_marginal_grid()`.
* Show posterior samples of `sigma` in `brm_marginal_draws()` and `brm_marginal_summaries()`.
* Allow `outcome = "response"` with `reference_time = NULL`. Sometimes raw response is analyzed but the data has no baseline time point.
* Preserve factors in `brm_data()` and encourage ordered factors for the time variable (#113).
* Add `brm_data_chronologize()` to ensure the correctness of the time variable.
* Do not drop columns in `brm_data()`. This helps `brm_data_chronologize()` operate correctly after calls to `brm_data()`.
* Add new elements `brms.mmrm_data` and `brms.mmrm_formula` to the `brms` fitted model object returned by `brm_model()`.
* Take defaults `data` and `formula` from the above in `brm_marginal_draws()`.
* Set the default value of `effect_size` to `attr(formula, "brm_allow_effect_size")`.
* Remove defaults from some arguments to `brm_data()` and document examples.
* Deprecate the `role` argument of `brm_data()` in favor of `reference_time` (#119).
* Add a new `model_missing_outcomes` in `brm_formula()` to optionally impute missing values during model fitting as described at <https://paulbuerkner.com/brms/articles/brms_missings.html> (#121).
* Add a new `imputed` argument to accept a `mice` multiply imputed dataset ("mids") in `brm_model()` (#121).
* Add a `summary()` method for `brm_transform_marginal()` objects.
* Do not recheck the rank of the formula in `brm_transform_marginal()`.
* Support constrained longitudinal data analysis (cLDA) for informative prior archetypes `brm_archetype_cells()`, `brm_archetype_effects()`,  `brm_archetype_successive_cells()`, and  `brm_archetype_successive_effects()` (#125). We cannot support cLDA for `brm_archetype_average_cells()` or  `brm_archetype_average_effects()` because then some parameters would no longer be averages of others.

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
