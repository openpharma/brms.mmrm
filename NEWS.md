# brms.mmrm 0.0.2.9003 (development)

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
* Allow `brm_plot_compare()` to select the x axis variable and faceting variables.

# brms.mmrm 0.0.2

* Fix grammatical issues in the description.

# brms.mmrm 0.0.1

* First version.
