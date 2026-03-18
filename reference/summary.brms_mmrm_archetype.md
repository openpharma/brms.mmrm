# Summarize an informative prior archetype.

For an informative prior archetype, show the transformation from model
parameters to marginal means.

## Usage

``` r
# S3 method for class 'brms_mmrm_archetype'
summary(object, message = TRUE, ...)
```

## Arguments

- object:

  The informative prior archetype to summarize.

- message:

  TRUE to print an informative message about the archetype and invisibly
  return a character vector of equations. `FALSE` to forgo verbose
  messages and non-invisibly return the equations.

- ...:

  Not used, but required for S3 methods that inherit from the base
  generic [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

Return a character vector with linear equations that map model
parameters to marginal means. If the `message` argument is `TRUE`
(default) then this character vector is returned invisibly and a verbose
description of the equations is printed.

## Examples

``` r
data <- brm_simulate_outline(
  n_group = 2,
  n_patient = 100,
  n_time = 4,
  rate_dropout = 0,
  rate_lapse = 0
) |>
  dplyr::mutate(response = rnorm(n = dplyr::n())) |>
  brm_data_change() |>
  brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
  brm_simulate_categorical(
    names = c("status1", "status2"),
    levels = c("present", "absent")
  )
dplyr::select(
  data,
  group,
  time,
  patient,
  starts_with("biomarker"),
  starts_with("status")
)
#> # A tibble: 600 × 7
#>    group   time   patient     biomarker1 biomarker2 status1 status2
#>    <chr>   <chr>  <chr>            <dbl>      <dbl> <chr>   <chr>  
#>  1 group_1 time_2 patient_001     1.11       -0.222 present absent 
#>  2 group_1 time_3 patient_001     1.11       -0.222 present absent 
#>  3 group_1 time_4 patient_001     1.11       -0.222 present absent 
#>  4 group_1 time_2 patient_002    -0.0254      1.52  absent  absent 
#>  5 group_1 time_3 patient_002    -0.0254      1.52  absent  absent 
#>  6 group_1 time_4 patient_002    -0.0254      1.52  absent  absent 
#>  7 group_1 time_2 patient_003     0.0805     -0.139 absent  present
#>  8 group_1 time_3 patient_003     0.0805     -0.139 absent  present
#>  9 group_1 time_4 patient_003     0.0805     -0.139 absent  present
#> 10 group_1 time_2 patient_004     0.0910     -1.54  present absent 
#> # ℹ 590 more rows
archetype <- brm_archetype_successive_cells(data)
equations <- summary(archetype)
#> # This is the "successive cells" informative prior archetype in brms.mmrm.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and important fixed effect parameters
#> # (right-hand side). Nuisance parameters are omitted.
#> # 
#> #   group_1:time_2 = x_group_1_time_2
#> #   group_1:time_3 = x_group_1_time_2 + x_group_1_time_3
#> #   group_1:time_4 = x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4
#> #   group_2:time_2 = x_group_2_time_2
#> #   group_2:time_3 = x_group_2_time_2 + x_group_2_time_3
#> #   group_2:time_4 = x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4
print(equations)
#> [1] "group_1:time_2 = x_group_1_time_2"                                      
#> [2] "group_1:time_3 = x_group_1_time_2 + x_group_1_time_3"                   
#> [3] "group_1:time_4 = x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4"
#> [4] "group_2:time_2 = x_group_2_time_2"                                      
#> [5] "group_2:time_3 = x_group_2_time_2 + x_group_2_time_3"                   
#> [6] "group_2:time_4 = x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4"
summary(archetype, message = FALSE)
#> [1] "group_1:time_2 = x_group_1_time_2"                                      
#> [2] "group_1:time_3 = x_group_1_time_2 + x_group_1_time_3"                   
#> [3] "group_1:time_4 = x_group_1_time_2 + x_group_1_time_3 + x_group_1_time_4"
#> [4] "group_2:time_2 = x_group_2_time_2"                                      
#> [5] "group_2:time_3 = x_group_2_time_2 + x_group_2_time_3"                   
#> [6] "group_2:time_4 = x_group_2_time_2 + x_group_2_time_3 + x_group_2_time_4"
```
