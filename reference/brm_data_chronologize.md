# Chronologize a dataset

Convert the discrete time variable into an ordered factor.

## Usage

``` r
brm_data_chronologize(
  data,
  order = NULL,
  levels = NULL,
  time = attr(data, "brm_time")
)
```

## Arguments

- data:

  Data frame or tibble with longitudinal data.

- order:

  Optional character string with the name of a variable in the data for
  ordering the time variable. Either `order` or `levels` must be
  supplied, but not both together. If `order` is supplied, the levels of
  `data[[order]]` must have a 1:1 correspondence with those of
  `data[[time]]`, and `sort(unique(data[[order]]))` must reflect the
  desired order of the levels of `data[[time]]`. For example, suppose
  you have a CDISC dataset with categorical time variable `AVISIT` and
  integer variable `AVISITN`. Then,
  `brm_data_chronologize(time = "AVISIT", order = "AVISITN")` will turn
  `AVISIT` into an ordered factor with levels that respect the ordering
  in `AVISITN`.

- levels:

  Optional character vector of levels of `data[[time]]` in chronological
  order. Used to turn `data[[time]]` into an ordered factor. Either
  `order` or `levels` must be supplied, but not both together.

- time:

  Character string with the name of the discrete time variable in the
  data. This is the variable that `brm_data_chronologize()` turns into
  an ordered factor. It needs to be specified explicitly if and only if
  the `data` argument was not produced by a call to
  [`brm_data()`](brm_data.md).

## Value

A data frame with the time column as an ordered factor.

## Details

Most MMRMs should use an ordered factor for the `time` column in the
data. This way, individual time points are treated as distinct factor
levels for the purposes of fixed effect parameterizations (see the
"Contrasts" section), and the explicit ordering ensures that informative
prior archetypes and ARMA-like correlation structures are expressed
correctly. Without the ordering, problems can arise when character
vectors are sorted: e.g. if `AVISIT` has levels
`"VISIT1", "VISIT2", ..., "VISIT10"`, then `brms` will mistake the the
order of scheduled study visits to be
`"VISIT1", "VISIT10", "VISIT2", ...`, which is not chronological.

You can easily turn the time variable into an ordered factor using
`brm_data_chronologize()`. Either supply an explicit character vector of
chronologically-ordered factor levels in the `levels` argument, or
supply the name of a time-ordered variable in the `order` argument.

`brm_data_chronologize()` can be called either before or just after
[`brm_data()`](brm_data.md), but in the former case, the discrete time
variable needs to be specified explicitly in `time` argument. And in the
latter, `brm_data_chronologize()` must be called before any of the
informative prior archetype functions such as
[`brm_archetype_successive_cells()`](brm_archetype_successive_cells.md).

## Contrasts

Ordinarily, ordered factors automatically use polynomial contrasts from
[`contr.poly()`](https://rdrr.io/r/stats/contrast.html). This is
undesirable for MMRMs, so if the time variable is an ordered factor,
then [`brm_data()`](brm_data.md) manually sets `contrasts(data[[time]])`
to a set of treatment contrasts using
[`contr.treatment()`](https://rdrr.io/r/stats/contrast.html). If you
prefer different contrasts, please manually set
`contrasts(data[[time]])` to something else after calling
[`brm_data()`](brm_data.md).

## See also

Other data: [`brm_data()`](brm_data.md),
[`brm_data_change()`](brm_data_change.md)

## Examples

``` r
data <- brm_simulate_outline(n_time = 12, n_patient = 4)
data$AVISIT <- gsub("_0", "_", data$time)
data$AVISITN <- as.integer(gsub("time_", "", data$time))
data[, c("AVISIT", "AVISITN")]
#> # A tibble: 96 × 2
#>    AVISIT  AVISITN
#>    <chr>     <int>
#>  1 time_1        1
#>  2 time_2        2
#>  3 time_3        3
#>  4 time_4        4
#>  5 time_5        5
#>  6 time_6        6
#>  7 time_7        7
#>  8 time_8        8
#>  9 time_9        9
#> 10 time_10      10
#> # ℹ 86 more rows
sort(unique(data$AVISIT)) # wrong order
#>  [1] "time_1"  "time_10" "time_11" "time_12" "time_2"  "time_3"  "time_4" 
#>  [8] "time_5"  "time_6"  "time_7"  "time_8"  "time_9" 
data1 <- brm_data_chronologize(data, time = "AVISIT", order = "AVISITN")
sort(unique(data1$AVISIT)) # correct order
#>  [1] time_1  time_2  time_3  time_4  time_5  time_6  time_7  time_8  time_9 
#> [10] time_10 time_11 time_12
#> 12 Levels: time_1 < time_2 < time_3 < time_4 < time_5 < time_6 < ... < time_12
levels <- paste0("time_", seq_len(12))
data2 <- brm_data_chronologize(data, time = "AVISIT", levels = levels)
sort(unique(data2$AVISIT)) # correct order
#>  [1] time_1  time_2  time_3  time_4  time_5  time_6  time_7  time_8  time_9 
#> [10] time_10 time_11 time_12
#> 12 Levels: time_1 < time_2 < time_3 < time_4 < time_5 < time_6 < ... < time_12
```
