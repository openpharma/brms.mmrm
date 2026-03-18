# Visualize posterior draws of marginals.

Visualize posterior draws of marginals.

## Usage

``` r
brm_plot_draws(draws, axis = "time", facet = c("group", "subgroup"))
```

## Arguments

- draws:

  A data frame of draws from an element of the output list of
  [`brm_marginal_summaries()`](brm_marginal_summaries.md).

- axis:

  Character of length 1 identifying the quantity to put on the
  horizontal axis. Must be be one of `"time"` or `"group"` if the
  marginal summaries are not subgroup-specific. If the marginals are
  subgroup-specific, then `axis` must be one of `"time"`, `"group"`, or
  `"subgroup"`.

- facet:

  Character vector of length 1 or 2 with quantities to generate facets.
  Each element must be `"time"`, `"group"`, or `"subgroup"`, and
  `c(axis, facet)` must all have unique elements. `"subgroup"` is
  automatically removed if the marginals have no subgroup. If `facet`
  has length 1, then faceting is wrapped. If `facet` has length 2, then
  faceting is in a grid, and the first element is horizontal facet.

## Value

A `ggplot` object.

## See also

Other visualization: [`brm_plot_compare()`](brm_plot_compare.md)

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
brm_plot_draws(draws = draws$difference_time)
}
```
