# brms.mmrm 0.0.2.9002 (development)

* Deprecate `brm_simulate()` in favor of `brm_simulate_simple()` (#3). The latter has a more specific name to disambiguate it from other simulation functions, and its parameterization conforms to the one in the methods vignette.
* Add new functions for nuanced simulations: `brm_simulate_outline()`, `brm_simulate_continuous()`, `brm_simulate_categorical()` (#3).
* In `brm_model()`, remove rows with missing responses. These rows are automatically removed by `brms` anyway, and by handling by handling this in `brms.mmrm`, we avoid a warning.
* Add subgroup analysis functionality (#18).
* In `brm_data()`, deprecate `level_control` in favor of `reference_group`.
* In `brm_data()`, deprecate `level_baseline` in favor of `reference_time`.

# brms.mmrm 0.0.2

* Fix grammatical issues in the description.

# brms.mmrm 0.0.1

* First version.
