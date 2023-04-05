---
title: "Interface package specification"
author: "Will Landau, Kevin Kunzmann, Yoni Sidi, Christian Stock"
format:
  html:
    toc: true
    toc-depth: 3
    number-sections: true
---

```{r, include = FALSE}
knitr::opts_chunk$set(eval = FALSE)
```

# Introduction

This document is the draft specification for `brms.mmrm`.

# Data functions

## `brm_data_preprocess()`

This function will be a general-purpose pipe-friendly data preprocessing function which accepts a simple tibble and returns a simple tibble. It will call a sequence of more specific pipe-friendly functions exposed to the user for individual tasks including but not limited to dropping patients with no observed post-baseline response, inserting explicit rows for implicitly missing responses, and sorting rows by patient and timepoint. 

### Signature

```{r}
brm_data_preprocess(
  data,
  response = "CHG",
  group = "TRT01P",
  patient = "USUBJID",
  rep = "AVISITN",
  baseline = "BASE",
  covariates = character(0)
)
```

### Value

A preprocessed tibble with the data to analyze.

### Arguments

* `data`: A tidy long-form data frame with one row per patient per rep and columns for the response, treatment group indicator, patient ID, rep, and optional baseline covariates.
* `response`: name of the column in `data` with the response variable.
* `group`: name of the column in `data` with the treatment group indicator.
* `baseline`: name of the column in `data` with the response at baseline. Should it be optional?
* `covariates`: character vector of names of columns in `data` with baseline covariates.

## Recipe methods

It is possible to implement methods in this package which extend the existing `recipes` package to make data processing easier and more conveneint for MMRMs for users already familiar with `recipes`. <https://github.com/tidymodels/themis> is an example of such an implementation for a different scenario.

## `brm_data_validate()`

Run assertions on the dataset to verify it is clean enough to analyze with the MMRM. This function will be exposed to the user and can be run after a successful call to `brm_data_preprocess()`. In addition, the function that runs the MMRM will run it internally to check the data. Possible assertions may include:

* The appropriate columns exist and have correct mode/type.
* Only `response` should be allowed to have `NA`'s.
* Rows within each patient are adjacent, rows are sorted by rep within patient.
* Every patient has a complete and unique set of discrete visits / timepoints.

### Signature

```{r}
brm_data_validate(
  data,
  response = "CHG",
  group = "TRT01P",
  patient = "USUBJID",
  rep = "AVISITN",
  baseline = "BASE",
  covariates = character(0)
)
```

### Arguments

Same as `brm_data_preprocess()`.

### Value

If there are no problems with the data, this function should invisibly return `NULL`. If problems are detected, the function should return an informative classed error, i.e. `rlang::abort(message = "something informative", class = "<PACKAGE_NAME>_error")`.

## `brm_simulate()`

Simulate a dataset from the prior predictive distribution of a basic MMRM with random effects marginalized out. Use `brms` to do this. This function corresponds to the `brm_model()` discussed later.

### Signature

```{r}
brm_model(
  data,
  formula = ~ ...,
  priors = ...,
  n_groups = 2,
  n_patients = 100,
  n_times = 4,
  n_binary = 0,
  n_continuous = 0,
  dropout = 0.1,
  ...
)
```

### Value

A list with 2 elements:

* `data`: a tidy `tibble` with the simulated data.
* `parameters`: a named list of model parameters simulated from the prior and used to simulate the data.

### Arguments

* `data`: preprocessed data object from `brm_data_preprocess()`.
* `formula`: an R formula to describe the relationships among the variables in the data. Internally, this formula will undergo a series of assertions for validation purposes, and then it will be passed to `brms`. Both the fixed effects parameterization and the covariance structure are specified in the formula.
* `priors` a prior specification, either using `brms::prior(...) + brms::prior(...) + ...` from `brms`, or a simplified specification using a special DSL in this package.
* `n_groups`: number of treatment groups to simulate.
* `n_patients`: number of patients to simulate.
* `n_times`: number of discrete timepoints to simulate.
* `n_binary`: number of binary covariates to simulate.
* `n_continuous`: number of continuous covariates to simulate.
* `dropout`: a number between 0 and 1 with the dropout rate to simulate at the final time point. These dropouts simulate discontinuation as a Poisson process where a patient is equally likely to drop out at each post-baseline visit. When a patient drops out, earlier timepoints are observed, and the current and later timepoints are unobserved/missing. Earlier timepoints may have fewer missing responses than `dropout`, and the number of missing responses at the final timepoint should be `dropout` on average.
* `...`: additional named arguments to `brms`.

# Model functions

Each kind of model will have its own user-side exported function. We will start with a basic MMRM with random effects marginalized out.

## `brm_model()`

Fit a basic MMRM with random effects marginalized out.

### Priors

It is an open question how we specify priors for `brms`. It would be nice to use something simpler and more situation-specific than `brms::prior(...) + brms::prior(...) + ...`.

### Signature

```{r}
brm_model(
  data,
  formula = ~ ...,
  priors = ...,
  n_chains = 4,
  n_workers = 1,
  n_warmup = 2e3,
  n_iterations = 4e3,
  ...
)
```

### Value

A classed object (S7?) with the `brms` fitted model object.

### Arguments

* `data`: preprocessed data object from `brm_data_preprocess()`.
* `formula`: an R formula to describe the relationships among the variables in the data. Internally, this formula will undergo a series of assertions for validation purposes, and then it will be passed to `brms`. Both the fixed effects parameterization and the covariance structure are specified in the formula.
* `priors` a prior specification, either using `brms::prior(...) + brms::prior(...) + ...` from `brms`, or a simplified specification using a special DSL in this package.
* `n_chains`: number of MCMC chains.
* `n_workers`: number of local worker processes for running the chains in parallel.
* `n_warmup`: number of MCMC warmup iterations per chain.
* `n_iterations`: number of saved MCMC iterations per chain.
* `...`: additional named arguments to `brms::brm(`.

# Results

## `brm_summary()`

S7-compatible method to conduct inference on the marginal posterior using `emmeans`.

### Signature

We consider two S7-compatible methods: one for models fit with the package, and another for MMRMs fit directly by interfacting with `brms`.

```{r}
brm_summary.brm_model(
  object,
  data,
  response_is_change = TRUE,
  group_reference = "Placebo",
  interval_level = 0.95,
  effect_threshold = 0,
  effect_comparison = ">",
  ...
)
```


```{r}
brm_summary.brms(
  object,
  data,
  response_is_change = TRUE,
  group_reference = "Placebo",
  interval_level = 0.95,
  effect_threshold = 0,
  effect_comparison = ">",
  ...
)
```

### Value

A data frame with one row per combination of treatment group and discrete time point (rep). Columns may include:

* Statistical summaries of the data, including observed means, standard deviations, number of observations, and number of non-missing observations.
* Posterior means, standard deviations, and quantiles of the marginal means of:
    * Response.
    * Change from baseline, if the response is `AVAL` and not `CHG`.
    * Treatment effect.
    * Effect size: treatment effect divided by the residual standard deviation.
* Posterior probabilities of the form Prob(treatment_effect > effect_threshold | data).

### Arguments

* `object`: either classed model object from `brm_model()` or an MMRM fit directly with `brms`.
* `data`: data frame returned from `brm_data_preprocess()` (and ideally validated with `brm_data_validate()`).
* `response_is_change`: logical, whether the response variable is change from baseline. If `FALSE`, then inference will be conducted on the response as if it were `AVAL`, then inference for the contrasts that represent change from baseline, then inference on the treatment difference. If `FALSE`, then explicit contrasts for change from baseline are omitted.
* `group_reference`: Level of `data$group` with the control group.
* `interval_level`: Credible level for posterior intervals.
* `effect_threshold`: Vector of effect thresholds for posterior probabilities of the form Prob(treatment_effect > effect_threshold | data).
* `effect_comparison`: Character vector with elements ">" or "<" denoting the comparison between the treatment effect and effect threshold in posterior probabilities of the form Prob(treatment_effect > effect_threshold | data). Must be the same length as `effect_threshold`.
* `...`: not used but still required by S3.

## `brm_plot()` and `plot()`

S7-compatible method to plot intervals of the fitted model marginal posterior against intervals on the data. For now this section shows only one possible plot we could choose for the `plot()` method, but there are others we could consider, e.g. posterior density plots from `ggdist`.

### Signature

```{r}
brm_plot(model, data)
plot.brm_model(x, y = NULL, data, ...) # model fit with the package
plot.brms(x, y = NULL, data, ...) # mmrm fit directly with brms
```

### Value

A `ggplot2` object plotting the fitted model marginal posterior (estimated marginal means and posterior intervals of the response) against the data (point estimates and confidence intervals).

### Arguments

* `x`: A classed model object from `brm_model()` or an MMRM fit directly with `brms`.
* `y`: Ignored (but required in the generic signature).
* `data`: preprocessed data frame from `brm_data_preprocess()`.
* `...`: ignored.

## `brm_convergence()`

Show concise summaries of basic convergence diagnostics for a fitted `brms` model. The documentation of the function should include background information on the convergence diagnostics to help users determine whether the MCMC succeeded, along with recommendations to address any specific issues with convergence that the diagnostics discover.

### Signature

```{r}
brm_convergence.brm_model(model) # model fit with the package
brm_convergence.brms(model) # mmrm fit directly with brms
```

### Value

A one-row data frame with columns for maximum Rhat, minimum bulk ESS, minimum tail ESS, and number of divergent transitions.

### Arguments

* `model`: model object from `brm_model()` or MMRM fit with `brms`.

# Additional functionality

* Explicit random effects.
* Historical borrowing via MAP/MAC and `RBesT`.
* Bayesian subgroup analysis.

# Questions

* Are there still plans to publicly release the Novartis {brms} MMRM vignettes?
*	Should we create a private GitHub repo for the upcoming package, along with issues for the questions below?
*	What should be the package name?
*	For the models, which simplifications can we make to the `brms` interface? E.g. for priors, 
it may be nice to use something simpler and more situation-specific than `brms::prior(...) + brms::prior(...) + ...`.
*	Which summary tables and plots would be useful?
