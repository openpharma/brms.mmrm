# Average marginal MCMC draws across time points.

Simple un-weighted arithmetic mean of marginal MCMC draws across time
points.

## Usage

``` r
brm_marginal_draws_average(draws, data, times = NULL, label = "average")
```

## Arguments

- draws:

  List of posterior draws from
  [`brm_marginal_draws()`](brm_marginal_draws.md).

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- times:

  Character vector of discrete time point levels over which to average
  the MCMC samples within treatment group levels. Set to `NULL` to
  average across all time points. Levels are automatically sanitized
  with `make.names(unique = FALSE, allow_ = TRUE)` to ensure agreement
  with `brms` variable names in downstream computations.

- label:

  Character of length 1, time point label for the averages.
  Automatically sanitized with
  `make.names(unique = FALSE, allow_ = TRUE)`. Must not conflict with
  any existing time point labels in the data after the label and time
  points are sanitized.

## Value

A named list of tibbles of MCMC draws of the marginal posterior
distribution of each treatment group and time point (or
group-by-subgroup-by-time, if applicable). See
[`brm_marginal_draws()`](brm_marginal_draws.md) for the full details of
the return value. The only difference is that
`brm_marginal_draws_average()` returns a single pseudo-time-point to
represent the average across multiple real time points.

## Separation string

Post-processing in [`brm_marginal_draws()`](brm_marginal_draws.md) names
each of the group-by-time marginal means with the delimiting character
string from `Sys.getenv("BRM_SEP", unset = "|")`. Neither the column
names nor element names of the group and time variables can contain this
string. To set a custom string yourself, use
`Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")`.

## See also

Other marginals: [`brm_marginal_data()`](brm_marginal_data.md),
[`brm_marginal_draws()`](brm_marginal_draws.md),
[`brm_marginal_grid()`](brm_marginal_grid.md),
[`brm_marginal_probabilities()`](brm_marginal_probabilities.md),
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
brm_marginal_draws_average(draws = draws, data = data)
brm_marginal_draws_average(
  draws = draws,
  data = data,
  times = c("time_1", "time_2"),
  label = "mean"
)
}
```
