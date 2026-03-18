# Summarize marginal transform.

Summarize a transformation from model parameters to marginal means.

## Usage

``` r
# S3 method for class 'brms_mmrm_transform_marginal'
summary(object, message = TRUE, ...)
```

## Arguments

- object:

  The [`brm_transform_marginal()`](brm_transform_marginal.md) matrix to
  summarize.

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
transform <- brm_transform_marginal(data = data, formula = formula)
equations <- summary(transform)
#> # This is a matrix to transform model parameters to marginal means.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and fixed effect parameters
#> # (right-hand side).
#> # 
#> #   group_1:time_1 = b_Intercept
#> #   group_1:time_2 = b_Intercept + b_timetime_2
#> #   group_1:time_3 = b_Intercept + b_timetime_3
#> #   group_1:time_4 = b_Intercept + b_timetime_4
#> #   group_2:time_1 = b_Intercept + b_groupgroup_2
#> #   group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2
#> #   group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3
#> #   group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4
print(equations)
#> [1] "group_1:time_1 = b_Intercept"                                                            
#> [2] "group_1:time_2 = b_Intercept + b_timetime_2"                                             
#> [3] "group_1:time_3 = b_Intercept + b_timetime_3"                                             
#> [4] "group_1:time_4 = b_Intercept + b_timetime_4"                                             
#> [5] "group_2:time_1 = b_Intercept + b_groupgroup_2"                                           
#> [6] "group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2"
#> [7] "group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3"
#> [8] "group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4"
summary(transform, message = FALSE)
#> [1] "group_1:time_1 = b_Intercept"                                                            
#> [2] "group_1:time_2 = b_Intercept + b_timetime_2"                                             
#> [3] "group_1:time_3 = b_Intercept + b_timetime_3"                                             
#> [4] "group_1:time_4 = b_Intercept + b_timetime_4"                                             
#> [5] "group_2:time_1 = b_Intercept + b_groupgroup_2"                                           
#> [6] "group_2:time_2 = b_Intercept + b_groupgroup_2 + b_timetime_2 + b_groupgroup_2:timetime_2"
#> [7] "group_2:time_3 = b_Intercept + b_groupgroup_2 + b_timetime_3 + b_groupgroup_2:timetime_3"
#> [8] "group_2:time_4 = b_Intercept + b_groupgroup_2 + b_timetime_4 + b_groupgroup_2:timetime_4"
class(transform)
#> [1] "brms_mmrm_transform_marginal" "matrix"                      
#> [3] "array"                       
print(transform)
#>                b_Intercept b_groupgroup_2 b_timetime_2 b_timetime_3
#> group_1|time_1           1              0            0            0
#> group_1|time_2           1              0            1            0
#> group_1|time_3           1              0            0            1
#> group_1|time_4           1              0            0            0
#> group_2|time_1           1              1            0            0
#> group_2|time_2           1              1            1            0
#> group_2|time_3           1              1            0            1
#> group_2|time_4           1              1            0            0
#>                b_timetime_4 b_groupgroup_2:timetime_2 b_groupgroup_2:timetime_3
#> group_1|time_1            0                         0                         0
#> group_1|time_2            0                         0                         0
#> group_1|time_3            0                         0                         0
#> group_1|time_4            1                         0                         0
#> group_2|time_1            0                         0                         0
#> group_2|time_2            0                         1                         0
#> group_2|time_3            0                         0                         1
#> group_2|time_4            1                         0                         0
#>                b_groupgroup_2:timetime_4
#> group_1|time_1                         0
#> group_1|time_2                         0
#> group_1|time_3                         0
#> group_1|time_4                         0
#> group_2|time_1                         0
#> group_2|time_2                         0
#> group_2|time_3                         0
#> group_2|time_4                         1
```
