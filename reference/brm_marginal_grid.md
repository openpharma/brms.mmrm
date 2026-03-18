# Marginal names grid.

Describe the column names of the data frames output by
[`brm_marginal_draws()`](brm_marginal_draws.md).

## Usage

``` r
brm_marginal_grid(data, formula)
```

## Arguments

- data:

  A classed data frame from [`brm_data()`](brm_data.md), or an
  informative prior archetype from a function like
  [`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

- formula:

  An object of class `"brmsformula"` from
  [`brm_formula()`](brm_formula.md) or
  [`brms::brmsformula()`](https://paulbuerkner.com/brms/reference/brmsformula.html).
  Should include the full mapping of the model, including fixed effects,
  residual correlation, and heterogeneity in the discrete-time-specific
  residual variance components.

## Value

A data frame with a `name` column with the names of columns of data
frames in [`brm_marginal_draws()`](brm_marginal_draws.md), along with
metadata to describe which groups, subgroups, and time points those
columns correspond to.

## Details

Useful for creating custom posterior summaries from the draws.

## See also

Other marginals: [`brm_marginal_data()`](brm_marginal_data.md),
[`brm_marginal_draws()`](brm_marginal_draws.md),
[`brm_marginal_draws_average()`](brm_marginal_draws_average.md),
[`brm_marginal_probabilities()`](brm_marginal_probabilities.md),
[`brm_marginal_summaries()`](brm_marginal_summaries.md)

## Examples

``` r
data <- brm_simulate_outline()
brm_marginal_grid(data, brm_formula(data))
#> # A tibble: 8 × 3
#>   name           group   time  
#>   <chr>          <chr>   <chr> 
#> 1 group_1|time_1 group_1 time_1
#> 2 group_1|time_2 group_1 time_2
#> 3 group_1|time_3 group_1 time_3
#> 4 group_1|time_4 group_1 time_4
#> 5 group_2|time_1 group_2 time_1
#> 6 group_2|time_2 group_2 time_2
#> 7 group_2|time_3 group_2 time_3
#> 8 group_2|time_4 group_2 time_4
data <- brm_simulate_outline(n_subgroup = 2L)
brm_marginal_grid(data, brm_formula(data))
#> # A tibble: 16 × 4
#>    name                      group   subgroup   time  
#>    <chr>                     <chr>   <chr>      <chr> 
#>  1 group_1|subgroup_1|time_1 group_1 subgroup_1 time_1
#>  2 group_1|subgroup_1|time_2 group_1 subgroup_1 time_2
#>  3 group_1|subgroup_1|time_3 group_1 subgroup_1 time_3
#>  4 group_1|subgroup_1|time_4 group_1 subgroup_1 time_4
#>  5 group_1|subgroup_2|time_1 group_1 subgroup_2 time_1
#>  6 group_1|subgroup_2|time_2 group_1 subgroup_2 time_2
#>  7 group_1|subgroup_2|time_3 group_1 subgroup_2 time_3
#>  8 group_1|subgroup_2|time_4 group_1 subgroup_2 time_4
#>  9 group_2|subgroup_1|time_1 group_2 subgroup_1 time_1
#> 10 group_2|subgroup_1|time_2 group_2 subgroup_1 time_2
#> 11 group_2|subgroup_1|time_3 group_2 subgroup_1 time_3
#> 12 group_2|subgroup_1|time_4 group_2 subgroup_1 time_4
#> 13 group_2|subgroup_2|time_1 group_2 subgroup_2 time_1
#> 14 group_2|subgroup_2|time_2 group_2 subgroup_2 time_2
#> 15 group_2|subgroup_2|time_3 group_2 subgroup_2 time_3
#> 16 group_2|subgroup_2|time_4 group_2 subgroup_2 time_4
```
