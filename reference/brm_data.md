# Create and preprocess an MMRM dataset.

Create a dataset to analyze with an MMRM.

## Usage

``` r
brm_data(
  data,
  outcome,
  baseline = NULL,
  group,
  subgroup = NULL,
  time,
  patient,
  covariates = character(0L),
  missing = NULL,
  reference_group,
  reference_subgroup = NULL,
  reference_time = NULL,
  role = NULL,
  level_baseline = NULL,
  level_control = NULL
)
```

## Arguments

- data:

  Data frame or tibble with longitudinal data.

- outcome:

  Character of length 1, name of the continuous outcome variable.
  Example possibilities from clinical trial datasets include `"CHG"` and
  `"AVAL"`. The `outcome` column in the data should be a numeric vector.

- baseline:

  Character of length 1, name of the baseline response variable (for
  example, `"BASE"` in many clinical trial datasets). Only relevant if
  the response variable is change from baseline. Supply `NULL` to ignore
  or omit.

- group:

  Character of length 1, name of the treatment group variable. Example
  possibilities from clinical trial datasets include `"TRT01P"`,
  `"TREATMENT"`, `"TRT"`, and `"GROUP"`. The `group` column in the data
  should be a character vector or unordered factor.

- subgroup:

  Character of length 1, optional name of the a discrete subgroup
  variable. Set to `NULL` to omit the subgroup (default). If present,
  the `subgroup` column in the data should be a character vector or
  unordered factor.

- time:

  Character of length 1, name of the discrete time variable. Example
  possibilities from clinical trial datasets include `"AVISIT"` and
  `"VISIT"`. For most analyses, please ensure the time column in the
  data is an ordered factor. You can easily turn the time variable into
  an ordered factor using
  [`brm_data_chronologize()`](brm_data_chronologize.md), either before
  or immediately after `brm_data()` (but before any `brm_archetype_*()`
  functions). This ensures the time points sort in chronological order,
  which ensures the correctness of informative prior archetypes and
  autoregressive / moving average correlation structures.

  Ordinarily, ordered factors automatically use polynomial contrasts
  from [`contr.poly()`](https://rdrr.io/r/stats/contrast.html). This is
  undesirable for MMRMs, so if the time variable is an ordered factor,
  then `brm_data()` manually sets `contrasts(data[[time]])` to a set of
  treatment contrasts using
  [`contr.treatment()`](https://rdrr.io/r/stats/contrast.html). If you
  prefer different contrasts, please manually set
  `contrasts(data[[time]])` to something else after calling
  `brm_data()`.

- patient:

  Character of length 1, name of the patient ID variable. Example
  possibilities from clinical trial datasets include `"USUBJID"`,
  `"SUBJID"`, `"PATIENT"`, `"PATIENTID"`, `"SUBJECT"`, `"SUBJIDID"`,
  `"SBJID"`, `"STYSID1A"`, `"SBJ1N"`, and `"ID"`. The `patient` column
  in the data should be a factor or character vector.

- covariates:

  Character vector of names of other covariates. All these covariates
  are assumed to be non-time-varying. For time-varying covariates,
  please manually expand the data to the full grid of patients and time
  points before you call `brm_data()`. See the "Preprocessing" section
  for details.

- missing:

  Character of length 1, name of an optional variable in a simulated
  dataset to indicate which outcome values should be missing. Set to
  `NULL` to omit.

- reference_group:

  Atomic value of length 1, Level of the `group` column to indicate the
  control group. Example possibilities from clinical trial datasets
  include `"Placebo"`, `"PLACEBO"`, `"PBO"`, `"PLB"`, `"CONTROL"`,
  `"CTRL"`, `"REFERENCE"`, and `"REF"`. `reference_group` only applies
  to the post-processing that happens in functions like
  [`brm_marginal_draws()`](brm_marginal_draws.md) downstream of the
  model. It does not control the fixed effect mapping in the model
  matrix that `brms` derives from the formula from
  [`brm_formula()`](brm_formula.md).

- reference_subgroup:

  Atomic value of length 1, level of the `subgroup` column to use as a
  reference for pairwise differences in when computing marginal means
  downstream of the model. It does not control the fixed effect mapping
  in the model matrix that `brms` derives from the formula from
  [`brm_formula()`](brm_formula.md).

- reference_time:

  Atomic value of length 1 or `NULL`, level of the `time` column to
  indicate the baseline time point. Leave as `NULL` if there is no
  baseline or baseline is not included in `data[[time]]`.

  If `reference_time` is not `NULL`, then
  [`brm_marginal_draws()`](brm_marginal_draws.md) will calculate change
  from baseline, and it will calculate treatment differences as
  differences between change-from-baseline values. If `reference_time`
  is not `NULL`, then [`brm_marginal_draws()`](brm_marginal_draws.md)
  will not calculate change from baseline, and it will calculate
  treatment differences as differences between response values.

  Note: `reference_time` only applies to the post-processing that
  happens in functions like
  [`brm_marginal_draws()`](brm_marginal_draws.md) downstream of the
  model. It does not control the fixed effect mapping in the model
  matrix that `brms` derives from the formula from
  [`brm_formula()`](brm_formula.md).

- role:

  Deprecated as unnecessary on 2024-07-11 (version 1.0.1.9007). Use
  `reference_time` to supply a baseline time point value if it exists.

- level_baseline:

  Deprecated on 2024-01-11 (version 0.2.0.9002). Use `reference_time`
  instead.

- level_control:

  Deprecated on 2024-01-11 (version 0.2.0.9002). Use `reference_group`
  instead.

## Value

A classed tibble with attributes which denote features of the data such
as the treatment group and discrete time variables.

## Preprocessing

The preprocessing steps in `brm_data()` are as follows:

- Perform basic assertions to make sure the data and other arguments are
  properly formatted.

- Convert the group and time columns to character vectors.

- Sanitize the levels of the group and time columns using
  `make.names(unique = FALSE, allow_ = TRUE)` to ensure agreement
  between the data and the output of `brms`.

- For each implicitly missing outcome observation, add explicit row with
  the outcome variable equal to `NA_real_`. Missing values in the
  predictors are implicitly filled using
  [`zoo::na.locf()`](https://rdrr.io/pkg/zoo/man/na.locf.html) on within
  each patient, which is not valid for time-varying covariates. If any
  covariates are time-varying, please manually perform this step before
  calling `brm_data()`.

- Arrange the rows of the data by group, then patient, then discrete
  time.

- Select only the columns of the data relevant to an MMRM analysis.

## Separation string

Post-processing in [`brm_marginal_draws()`](brm_marginal_draws.md) names
each of the group-by-time marginal means with the delimiting character
string from `Sys.getenv("BRM_SEP", unset = "|")`. Neither the column
names nor element names of the group and time variables can contain this
string. To set a custom string yourself, use
`Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")`.

## See also

Other data: [`brm_data_change()`](brm_data_change.md),
[`brm_data_chronologize()`](brm_data_chronologize.md)

## Examples

``` r
set.seed(0)
data <- brm_simulate_simple()$data
colnames(data) <- paste0("col_", colnames(data))
data
#> # A tibble: 800 × 4
#>    col_patient col_time col_response col_group
#>    <chr>       <chr>           <dbl> <chr>    
#>  1 patient_001 time_1          1.47  group_1  
#>  2 patient_001 time_2          3.10  group_1  
#>  3 patient_001 time_3          2.22  group_1  
#>  4 patient_001 time_4          0.215 group_1  
#>  5 patient_002 time_1          1.03  group_1  
#>  6 patient_002 time_2          2.28  group_1  
#>  7 patient_002 time_3          2.36  group_1  
#>  8 patient_002 time_4          2.33  group_1  
#>  9 patient_003 time_1          0.128 group_1  
#> 10 patient_003 time_2          1.89  group_1  
#> # ℹ 790 more rows
brm_data(
  data = data,
  outcome = "col_response",
  group = "col_group",
  time = "col_time",
  patient = "col_patient",
  reference_group = "group_1",
  reference_time = "time_1"
)
#> # A tibble: 800 × 4
#>    col_patient col_time col_response col_group
#>    <chr>       <chr>           <dbl> <chr>    
#>  1 patient_001 time_1          1.47  group_1  
#>  2 patient_001 time_2          3.10  group_1  
#>  3 patient_001 time_3          2.22  group_1  
#>  4 patient_001 time_4          0.215 group_1  
#>  5 patient_002 time_1          1.03  group_1  
#>  6 patient_002 time_2          2.28  group_1  
#>  7 patient_002 time_3          2.36  group_1  
#>  8 patient_002 time_4          2.33  group_1  
#>  9 patient_003 time_1          0.128 group_1  
#> 10 patient_003 time_2          1.89  group_1  
#> # ℹ 790 more rows
```
