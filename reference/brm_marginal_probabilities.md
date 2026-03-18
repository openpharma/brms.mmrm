# Marginal probabilities on the treatment effect for an MMRM.

Marginal probabilities on the treatment effect for an MMRM.

## Usage

``` r
brm_marginal_probabilities(draws, direction = "greater", threshold = 0)
```

## Arguments

- draws:

  Posterior draws of the marginal posterior obtained from
  [`brm_marginal_draws()`](brm_marginal_draws.md).

- direction:

  Character vector of the same length as `threshold`. `"greater"` to
  compute the marginal posterior probability that the treatment effect
  is greater than the threshold, `"less"` to compute the marginal
  posterior probability that the treatment effect is less than the
  threshold. Each element `direction[i]` corresponds to `threshold[i]`
  for all `i` from 1 to `length(direction)`.

- threshold:

  Numeric vector of the same length as `direction`, treatment effect
  threshold for computing posterior probabilities. Each element
  `direction[i]` corresponds to `threshold[i]` for all `i` from 1 to
  `length(direction)`.

## Value

A tibble of probabilities of the form
`Prob(treatment effect > threshold | data)` and/or
`Prob(treatment effect < threshold | data)`. It has one row per
probability and the following columns: \* `group`: treatment group. \*
`subgroup`: subgroup level, if applicable. \* `time`: discrete time
point, \* `direction`: direction of the comparison in the marginal
probability: `"greater"` for `>`, `"less"` for `<` \* `threshold`:
treatment effect threshold in the probability statement. \* `value`:
numeric value of the estimate of the probability.

## See also

Other marginals: [`brm_marginal_data()`](brm_marginal_data.md),
[`brm_marginal_draws()`](brm_marginal_draws.md),
[`brm_marginal_draws_average()`](brm_marginal_draws_average.md),
[`brm_marginal_grid()`](brm_marginal_grid.md),
[`brm_marginal_summaries()`](brm_marginal_summaries.md)

## Examples

``` r
if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
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
tmp <- utils::capture.output(
  suppressMessages(
    suppressWarnings(
      model <- brm_model(
        data = data,
        formula = formula,
        chains = 1,
        iter = 100,
        refresh = 0
      )
    )
  )
)
draws <- brm_marginal_draws(data = data, formula = formula, model = model)
brm_marginal_probabilities(draws, direction = "greater", threshold = 0)
}
```
