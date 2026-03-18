# Visually compare the marginals of multiple models and/or datasets.

Visually compare the marginals of multiple models and/or datasets.

## Usage

``` r
brm_plot_compare(
  ...,
  marginal = "response",
  compare = "source",
  axis = "time",
  facet = c("group", "subgroup")
)
```

## Arguments

- ...:

  Named `tibble`s of marginals posterior summaries from
  [`brm_marginal_summaries()`](brm_marginal_summaries.md) and/or
  [`brm_marginal_data()`](brm_marginal_data.md).

- marginal:

  Character of length 1, which kind of marginal to visualize. Must be a
  value in the `marginal` column of the supplied `tibble`s in the `...`
  argument. Only applies to MCMC output, the data is always on the scale
  of the response variable.

- compare:

  Character of length 1 identifying the variable to display using
  back-to-back interval plots of different colors. This is the primary
  comparison of interest. Must be one of `"source"` (the source of the
  marginal summaries, e.g. a model or dataset), `"time"` or `"group"`
  (in the non-subgroup case). Can also be `"subgroup"` if all the
  marginal summaries are subgroup-specific. The value must not be in
  `axis` or `facet`.

- axis:

  Character of length 1 identifying the quantity to put on the
  horizontal axis. Must be be one of `"source"` (the source of the
  marginal summaries, e.g. a model or dataset), `"time"`, or `"group"`
  (in the non-subgroup case). If the marginals are subgroup-specific,
  then `axis` can also be `"subgroup"`. The value must not be in
  `compare` or `facet`.

- facet:

  Character vector of length 1 or 2 with quantities to generate facets.
  Each element must be `"source"` (the source of the marginal summaries,
  e.g. a model or dataset), `"time"`, `"group"`, or `"subgroup"`, and
  `c(axis, facet)` must all have unique elements. `"subgroup"` is
  automatically removed if not all the marginal summaries have a
  subgroup column. If `facet` has length 1, then faceting is wrapped. If
  `facet` has length 2, then faceting is in a grid, and the first
  element is horizontal facet.

## Value

A `ggplot` object.

## Details

By default, `brm_plot_compare()` compares multiple models and/or
datasets side-by-side. The `compare` argument selects the primary
comparison of interest, and arguments `axis` and `facet` control the
arrangement of various other components of the plot. The subgroup
variable is automatically included if and only if all the supplied
marginal summaries have a subgroup column.

## See also

Other visualization: [`brm_plot_draws()`](brm_plot_draws.md)

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
suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
summaries_data <- brm_marginal_data(data)
brm_plot_compare(
  model1 = summaries_draws,
  model2 = summaries_draws,
  data = summaries_data
)
brm_plot_compare(
  model1 = summaries_draws,
  model2 = summaries_draws,
  marginal = "difference"
)
}
```
