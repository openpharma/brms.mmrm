# Label a prior with levels in the data.

Label an informative prior for a parameter using a collection of levels
in the data.

## Usage

``` r
brm_prior_label(label = NULL, code, group, subgroup = NULL, time)
```

## Arguments

- label:

  A `tibble` with the prior labeling scheme so far, with one row per
  model parameter and columns for the Stan code, treatment group,
  subgroup, and discrete time point of each parameter.

- code:

  Character of length 1, Stan code for the prior. Could be a string like
  `"normal(1, 2.2)"`. The full set of priors is given in the Stan
  Function Reference at <https://mc-stan.org/docs/functions-reference/>
  in the "Unbounded Continuous Distributions" section. See the
  documentation
  [`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  for more details.

- group:

  Value of length 1, level of the treatment group column in the data to
  label the prior. The treatment group column is the one you identified
  with the `group` argument of [`brm_data()`](brm_data.md).

- subgroup:

  Value of length 1, level of the subgroup column in the data to label
  the prior. The subgroup column is the one you identified with the
  `subgroup` argument of [`brm_data()`](brm_data.md), if applicable. Not
  every dataset has a subgroup variable. If yours does not, please
  either ignore this argument or set it to `NULL`.

- time:

  Value of length 1, level of the discrete time column in the data to
  label the prior. The discrete time column is the one you identified
  with the `time` argument of [`brm_data()`](brm_data.md).

## Value

A `tibble` with one row per model parameter and columns for the Stan
code, treatment group, subgroup, and discrete time point of each
parameter. You can supply this `tibble` to the `label` argument of
[`brm_prior_archetype()`](brm_prior_archetype.md).

## Prior labeling

Informative prior archetypes use a labeling scheme to assign priors to
fixed effects. How it works:

    1. First, assign the prior of each parameter a collection
      of labels from the data. This can be done manually or with
      successive calls to [brm_prior_label()].
    2. Supply the labeling scheme to [brm_prior_archetype()].
      [brm_prior_archetype()] uses attributes of the archetype
      to map labeled priors to their rightful parameters in the model.

For informative prior archetypes, this process is much more convenient
and robust than manually calling
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
However, it requires an understanding of how the labels of the priors
map to parameters in the model. This mapping varies from archetype to
archetype, and it is documented in the help pages of archetype-specific
functions such as
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

## See also

Other priors: [`brm_prior_archetype()`](brm_prior_archetype.md),
[`brm_prior_simple()`](brm_prior_simple.md),
[`brm_prior_template()`](brm_prior_template.md)

## Examples

``` r
set.seed(0L)
data <- brm_simulate_outline(
  n_group = 2,
  n_patient = 100,
  n_time = 3,
  rate_dropout = 0,
  rate_lapse = 0
) |>
  dplyr::mutate(response = rnorm(n = dplyr::n())) |>
  brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
  brm_simulate_categorical(
    names = c("status1", "status2"),
    levels = c("present", "absent")
  )
archetype <- brm_archetype_successive_cells(data)
dplyr::distinct(data, group, time)
#> # A tibble: 6 × 2
#>   group   time  
#>   <chr>   <chr> 
#> 1 group_1 time_1
#> 2 group_1 time_2
#> 3 group_1 time_3
#> 4 group_2 time_1
#> 5 group_2 time_2
#> 6 group_2 time_3
label <- NULL |>
  brm_prior_label("normal(1, 1)", group = "group_1", time = "time_1") |>
  brm_prior_label("normal(1, 2)", group = "group_1", time = "time_2") |>
  brm_prior_label("normal(1, 3)", group = "group_1", time = "time_3") |>
  brm_prior_label("normal(2, 1)", group = "group_2", time = "time_1") |>
  brm_prior_label("normal(2, 2)", group = "group_2", time = "time_2") |>
  brm_prior_label("normal(2, 3)", group = "group_2", time = "time_3")
label
#> # A tibble: 6 × 3
#>   code         group   time  
#>   <chr>        <chr>   <chr> 
#> 1 normal(1, 1) group_1 time_1
#> 2 normal(1, 2) group_1 time_2
#> 3 normal(1, 3) group_1 time_3
#> 4 normal(2, 1) group_2 time_1
#> 5 normal(2, 2) group_2 time_2
#> 6 normal(2, 3) group_2 time_3
```
