# Summary statistics of the marginal posterior of an MMRM.

Summary statistics of the marginal posterior of an MMRM.

## Usage

``` r
brm_marginal_summaries(draws, level = 0.95)
```

## Arguments

- draws:

  Posterior draws of the marginal posterior obtained from
  [`brm_marginal_draws()`](brm_marginal_draws.md).

- level:

  Numeric of length 1 between 0 and 1, credible level for the credible
  intervals.

## Value

A tibble with one row per summary statistic and the following columns:

- `marginal`: type of marginal distribution. If `outcome` was
  `"response"` in [`brm_marginal_draws()`](brm_marginal_draws.md), then
  possible values include `"response"` for the response on the raw
  scale, `"change"` for change from baseline, and `"difference"` for
  treatment difference in terms of change from baseline. If `outcome`
  was `"change"`, then possible values include `"response"` for the
  response one the change from baseline scale and `"difference"` for
  treatment difference.

- `statistic`: type of summary statistic. `"lower"` and `"upper"` are
  bounds of an equal-tailed quantile-based credible interval.

- `group`: treatment group.

- `subgroup`: subgroup level, if applicable.

- `time`: discrete time point.

- `value`: numeric value of the estimate.

- `mcse`: Monte Carlo standard error of the estimate. The `statistic`
  column has the following possible values:

- `mean`: posterior mean.

- `median`: posterior median.

- `sd`: posterior standard deviation of the mean.

- `lower`: lower bound of an equal-tailed credible interval of the mean,
  with credible level determined by the `level` argument.

- `upper`: upper bound of an equal-tailed credible interval with
  credible level determined by the `level` argument.

## See also

Other marginals: [`brm_marginal_data()`](brm_marginal_data.md),
[`brm_marginal_draws()`](brm_marginal_draws.md),
[`brm_marginal_draws_average()`](brm_marginal_draws_average.md),
[`brm_marginal_grid()`](brm_marginal_grid.md),
[`brm_marginal_probabilities()`](brm_marginal_probabilities.md)

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
suppressWarnings(brm_marginal_summaries(draws))
}
```
