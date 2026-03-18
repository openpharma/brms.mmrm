# Marginal summaries of the data.

Marginal summaries of the data.

## Usage

``` r
brm_marginal_data(
  data,
  level = 0.95,
  use_subgroup = !is.null(attr(data, "brm_subgroup"))
)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- level:

  Numeric of length 1 from 0 to 1, level of the confidence intervals.

- use_subgroup:

  Logical of length 1, whether to summarize the data by each subgroup
  level.

## Value

A tibble with one row per summary statistic and the following columns:

- `group`: treatment group.

- `subgroup`: subgroup level. Only included if the `subgroup` argument
  of `brm_marginal_data()` is `TRUE`.

- `time`: discrete time point.

- `statistic`: type of summary statistic.

- `value`: numeric value of the estimate.

The `statistic` column has the following possible values:

- `mean`: observed mean response after removing missing values.

- `median`: observed median response after removing missing values.

- `sd`: observed standard deviation of the response after removing
  missing values.

- `lower`: lower bound of a normal equal-tailed confidence interval with
  confidence level determined by the `level` argument.

- `upper`: upper bound of a normal equal-tailed confidence interval with
  confidence level determined by the `level` argument.

- `n_observe`: number of non-missing values in the response.

- `n_total`: number of total records in the data for the given
  group/time combination, including both observed and missing values.

## See also

Other marginals: [`brm_marginal_draws()`](brm_marginal_draws.md),
[`brm_marginal_draws_average()`](brm_marginal_draws_average.md),
[`brm_marginal_grid()`](brm_marginal_grid.md),
[`brm_marginal_probabilities()`](brm_marginal_probabilities.md),
[`brm_marginal_summaries()`](brm_marginal_summaries.md)

## Examples

``` r
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
brm_marginal_data(data = data)
#> # A tibble: 56 × 4
#>    statistic group   time    value
#>    <chr>     <chr>   <chr>   <dbl>
#>  1 lower     group_1 time_1  1.39 
#>  2 lower     group_1 time_2  2.73 
#>  3 lower     group_1 time_3  2.70 
#>  4 lower     group_1 time_4  1.88 
#>  5 lower     group_2 time_1 -0.118
#>  6 lower     group_2 time_2  1.23 
#>  7 lower     group_2 time_3  1.12 
#>  8 lower     group_2 time_4  0.300
#>  9 mean      group_1 time_1  1.23 
#> 10 mean      group_1 time_2  2.57 
#> # ℹ 46 more rows
```
