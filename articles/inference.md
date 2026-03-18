# Inference

This vignette explains how `brms.mmrm` conducts posterior inference on a
fitted [MMRM
model](https://openpharma.github.io/brms.mmrm/articles/model.html) using
estimated marginal means.

## Example data

Throughout this vignette, we use the `mmrm` package’s `fev_data`
dataset, a simulation of a clinical trial in which chronic obstructive
pulmonary disease (COPD) patients (variable `USUBJID`) were randomized
to different treatment groups (variable `ARMCD`) and measured across
four discrete time points (variable `AVISIT`). The given response
variable is forced expired volume in one second (`FEV1`), and we are
interested in the `FEV1` change from baseline to each time point
(derived variable `FEV_CHG`). For this vignette, we impute missing
responses in order to simplify the discussion.

``` r
library(brms.mmrm)
library(dplyr)
library(tidyr)
data(fev_data, package = "mmrm")
data <- fev_data |>
  group_by(USUBJID) |>
  complete(AVISIT) |>
  arrange(AVISIT) |>
  fill(
    any_of(c("ARMCD", "FEV1_BL", "RACE", "SEX", "WEIGHT")),
    .direction = "downup"
  ) |>
  mutate(FEV1 = na.locf(FEV1, na.rm = FALSE)) |>
  mutate(FEV1 = na.locf(FEV1, na.rm = FALSE, fromLast = TRUE)) |>
  ungroup() |>
  filter(!is.na(FEV1)) |>
  mutate(FEV1_CHG = FEV1 - FEV1_BL, USUBJID = as.character(USUBJID)) |>
  select(-FEV1) |>
  as_tibble() |>
  arrange(USUBJID, AVISIT) |>
  brm_data(
    outcome = "FEV1_CHG",
    baseline = "FEV1_BL",
    group = "ARMCD",
    patient = "USUBJID",
    time = "AVISIT",
    covariates = c("RACE", "SEX", "WEIGHT"),
    reference_group = "PBO",
    reference_time = "VIS1"
  )
```

``` r
data
#> # A tibble: 788 × 10
#>    USUBJID AVISIT ARMCD RACE        SEX   FEV1_BL WEIGHT VISITN VISITN2 FEV1_CHG
#>    <chr>   <fct>  <fct> <fct>       <fct>   <dbl>  <dbl>  <int>   <dbl>    <dbl>
#>  1 PT10    VIS1   PBO   Black or A… Fema…    57.7  0.795      1 -0.394    -12.7 
#>  2 PT10    VIS2   PBO   Black or A… Fema…    57.7  0.823      2 -0.0593   -12.7 
#>  3 PT10    VIS3   PBO   Black or A… Fema…    57.7  0.594      3  1.10     -12.7 
#>  4 PT10    VIS4   PBO   Black or A… Fema…    57.7  0.207      4  0.763    -12.7 
#>  5 PT100   VIS1   PBO   Black or A… Fema…    51.8  0.362      1  1.59     -17.2 
#>  6 PT100   VIS2   PBO   Black or A… Fema…    51.8  0.404      2  0.0450   -12.5 
#>  7 PT100   VIS3   PBO   Black or A… Fema…    51.8  0.504      3 -0.715    -11.2 
#>  8 PT100   VIS4   PBO   Black or A… Fema…    51.8  0.201      4  0.865    -11.2 
#>  9 PT102   VIS1   PBO   Asian       Fema…    52.2  0.577      1 -0.416    -16.6 
#> 10 PT102   VIS2   PBO   Asian       Fema…    52.2  0.227      2 -0.376     -8.48
#> # ℹ 778 more rows
```

## Marginal means for clinical trials

According to Lenth (2016), marginal means (formerly “least-squares
means”) are predictions (usually averaged predictions) at each point in
a reference grid. The reference grid declares combinations of levels of
factors of interest. In a clinical trial with repeated measures, we are
often interested in the mean response at each combination of treatment
group and discrete time point. For our FEV1 dataset, we are interested
in the mean of `FEV1_CHG` and its standard error for each combination of
treatment group and time point.[¹](#fn1) In other words, we want to
estimate the mean `FEV1_CHG` for group `"TRT"` time `"VIS1"`, the mean
`FEV1_CHG` for group `"TRT"` time `"VIS2"`, and so on.[²](#fn2) We
represent our goals in a reference with one row per marginal mean of
interest and columns with the levels of the factors of interest.

``` r
reference_grid <- distinct(data, ARMCD, AVISIT)
reference_grid
#> # A tibble: 8 × 2
#>   ARMCD AVISIT
#>   <fct> <fct> 
#> 1 PBO   VIS1  
#> 2 PBO   VIS2  
#> 3 PBO   VIS3  
#> 4 PBO   VIS4  
#> 5 TRT   VIS1  
#> 6 TRT   VIS2  
#> 7 TRT   VIS3  
#> 8 TRT   VIS4
```

It is seldom trivial to estimate marginal means. For example, the
following parameterization includes an intercept term, additive terms
for each level of each factor, interactions to capture non-additive
relationships among factors, continuous covariates, and different
`FEV1_BL` slopes for different time points. Here, there is no model
coefficient that directly corresponds to a marginal mean of interest.
Even terms like `AVISITVIS2:ARMCDTRT` implicitly condition on a subset
of the data because of the other variables involved.

``` r
brms_mmrm_formula <- brm_formula(data, correlation = "diagonal")
base_formula <- as.formula(brms_mmrm_formula[[1]])
attr(base_formula, "nl") <- NULL
attr(base_formula, "loop") <- NULL
```

``` r
base_formula
#> FEV1_CHG ~ FEV1_BL + FEV1_BL:AVISIT + ARMCD + ARMCD:AVISIT + 
#>     AVISIT + RACE + SEX + WEIGHT
#> attr(,"center")
#> [1] TRUE
#> <environment: 0x55d4f7ffe668>
```

``` r
colnames(model.matrix(object = base_formula, data = data))
#>  [1] "(Intercept)"                   "FEV1_BL"                      
#>  [3] "ARMCDTRT"                      "AVISITVIS2"                   
#>  [5] "AVISITVIS3"                    "AVISITVIS4"                   
#>  [7] "RACEBlack or African American" "RACEWhite"                    
#>  [9] "SEXFemale"                     "WEIGHT"                       
#> [11] "FEV1_BL:AVISITVIS2"            "FEV1_BL:AVISITVIS3"           
#> [13] "FEV1_BL:AVISITVIS4"            "AVISITVIS2:ARMCDTRT"          
#> [15] "AVISITVIS3:ARMCDTRT"           "AVISITVIS4:ARMCDTRT"
```

To accomplish our goals, we need to carefully construct a linear
transformation that maps these model coefficients to the marginal means
of interest. The transformation should evaluate contrasts on the
interesting parameters and average out the uninteresting parameters.

## Existing capabilities

[`brms.mmrm::brm_model()`](../reference/brm_model.md) returns a fitted
[`brms`](https://paulbuerkner.com/brms/) model, and
[`brms`](https://paulbuerkner.com/brms/) already has tools for posterior
inference. Through a combination of native functions and S3 methods,
[`brms`](https://paulbuerkner.com/brms/) integrates not only with
[`posterior`](https://mc-stan.org/posterior/) and
[`loo`](https://mc-stan.org/loo/), but also
[`emmeans`](https://cran.r-project.org/package=emmeans) for the
estimation of marginal means and downstream contrasts.

Despite the existing features in `brms`, `brms.mmrm` implements custom
code to transform model coefficients into marginal means. This is
because the reference grids in `emmeans` can only condition on factors
explicitly declared in the model formula supplied to `brms`, whereas
`brms.mmrm` needs more flexibility in order to support [informative
prior
archetypes](https://openpharma.github.io/brms.mmrm/articles/archetypes.html)
(Bedrick et al. (1996), Bedrick et al. (1997), Christensen et al.
(2010), Rosner et al. (2021)).

## How `brms.mmrm` estimates marginal means

To estimate marginal means,
[`brms.mmrm::brm_transform_marginal()`](../reference/brm_transform_marginal.md)
creates a special matrix.

``` r
transform <- brm_transform_marginal(data = data, formula = brms_mmrm_formula)
```

``` r
dim(transform)
#> [1]  8 16
transform[, 1:4]
#>          b_Intercept b_FEV1_BL b_ARMCDTRT b_AVISITVIS2
#> PBO|VIS1           1  40.12532          0            0
#> PBO|VIS2           1  40.12532          0            1
#> PBO|VIS3           1  40.12532          0            0
#> PBO|VIS4           1  40.12532          0            0
#> TRT|VIS1           1  40.12532          1            0
#> TRT|VIS2           1  40.12532          1            1
#> TRT|VIS3           1  40.12532          1            0
#> TRT|VIS4           1  40.12532          1            0
```

This special matrix encodes the equations below which map model
coefficients to marginal means.[³](#fn3)

``` r
summary(transform)
#> # This is a matrix to transform model parameters to marginal means.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and fixed effect parameters
#> # (right-hand side).
#> # 
#> #   PBO:VIS1 = b_Intercept + 40.13*b_FEV1_BL + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT
#> #   PBO:VIS2 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS2 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS2
#> #   PBO:VIS3 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS3 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS3
#> #   PBO:VIS4 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS4 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS4
#> #   TRT:VIS1 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT
#> #   TRT:VIS2 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS2 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS2 + b_ARMCDTRT:AVISITVIS2
#> #   TRT:VIS3 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS3 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS3 + b_ARMCDTRT:AVISITVIS3
#> #   TRT:VIS4 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS4 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS4 + b_ARMCDTRT:AVISITVIS4
```

Multiplying the matrix by a set of model coefficients is the same as
plugging the coefficients into the equations above. Both produce
estimates of marginal means.

``` r
model <- lm(formula = base_formula, data = data)
marginals_custom <- transform %*% coef(model)
marginals_custom
#>                [,1]
#> PBO|VIS1 -4.5998295
#> PBO|VIS2 -2.5445943
#> PBO|VIS3  0.9841880
#> PBO|VIS4  5.6013241
#> TRT|VIS1 -1.2858526
#> TRT|VIS2  0.8466639
#> TRT|VIS3  3.8011416
#> TRT|VIS4 10.0521521
```

This technique is similar to
[`emmeans::emmeans(weights = "proportional")`](https://cran.r-project.org/package=emmeans)[⁴](#fn4)
(Lenth (2016), Searle et al. (1980)) and produces similar estimates.

``` r
library(emmeans)
#> Welcome to emmeans.
#> Caution: You lose important information if you filter this package's results.
#> See '? untidy'
marginals_emmeans <- emmeans(
  object = model,
  specs = ~ARMCD:AVISIT,
  weights = "proportional",
  nuisance = c("USUBJID", "RACE", "SEX")
) |>
  as.data.frame() |>
  as_tibble() |>
  select(ARMCD, AVISIT, emmean) |>
  arrange(ARMCD, AVISIT)
```

``` r
marginals_emmeans
#> # A tibble: 8 × 3
#>   ARMCD AVISIT emmean
#>   <fct> <fct>   <dbl>
#> 1 PBO   VIS1   -4.60 
#> 2 PBO   VIS2   -2.54 
#> 3 PBO   VIS3    0.984
#> 4 PBO   VIS4    5.60 
#> 5 TRT   VIS1   -1.29 
#> 6 TRT   VIS2    0.847
#> 7 TRT   VIS3    3.80 
#> 8 TRT   VIS4   10.1
```

``` r
marginals_custom - marginals_emmeans$emmean
#>                   [,1]
#> PBO|VIS1  0.000000e+00
#> PBO|VIS2  4.440892e-16
#> PBO|VIS3 -1.110223e-16
#> PBO|VIS4 -8.881784e-16
#> TRT|VIS1 -1.110223e-15
#> TRT|VIS2 -5.551115e-16
#> TRT|VIS3 -1.332268e-15
#> TRT|VIS4 -1.776357e-15
```

For our Bayesian MMRMs in `brms.mmrm`, the transformation from
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
operates on each individual draw from the joint posterior distribution.
The transformation matrix produced by
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md) is
the value of the `transform` argument of
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md). That way,
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md) produces an
entire estimated posterior of each marginal mean, rather than point
estimates that assume a normal or Student-t distribution.[⁵](#fn5)

## How `brm_marginal_draws()` works

Let us take a closer look at the equations that map model parameters to
marginal means.

``` r
summary(transform)
#> # This is a matrix to transform model parameters to marginal means.
#> # The following equations show the relationships between the
#> # marginal means (left-hand side) and fixed effect parameters
#> # (right-hand side).
#> # 
#> #   PBO:VIS1 = b_Intercept + 40.13*b_FEV1_BL + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT
#> #   PBO:VIS2 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS2 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS2
#> #   PBO:VIS3 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS3 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS3
#> #   PBO:VIS4 = b_Intercept + 40.13*b_FEV1_BL + b_AVISITVIS4 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS4
#> #   TRT:VIS1 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT
#> #   TRT:VIS2 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS2 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS2 + b_ARMCDTRT:AVISITVIS2
#> #   TRT:VIS3 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS3 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS3 + b_ARMCDTRT:AVISITVIS3
#> #   TRT:VIS4 = b_Intercept + 40.13*b_FEV1_BL + b_ARMCDTRT + b_AVISITVIS4 + 0.38*b_RACEBlackorAfricanAmerican + 0.27*b_RACEWhite + 0.53*b_SEXFemale + 0.52*b_WEIGHT + 40.13*b_FEV1_BL:AVISITVIS4 + b_ARMCDTRT:AVISITVIS4
```

These equations include terms not only for the fixed effects of
interest, but also for nuisance variables `FEV1_BL`, `SEX`, `RACE`, and
`WEIGHT`. These nuisance variables were originally part of the model
formula, which means each marginal mean can only be interpreted relative
to a fixed value of `FEV1_BL`, a fixed proportion of female patients,
etc. For example, if we dropped `40.13*b_FEV1_BL`, then `PBO:VIS1` would
be the placebo mean at visit 1 for patients with `FEV1_BL = 0`: in other
words, patients who cannot breathe out any air from their lungs at the
beginning of the study. Similarly, if we dropped `0.53*b_SEXFemale`,
then we would have to interpret `PBO:VIS1` as the visit 1 placebo mean
for male patients only. Fixed values `40.13` and `0.53` are averages
over the data to ensure our marginal means apply to the entire patient
population as a whole.

The major challenge of
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md) is
to condition on nuisance values that represent appropriate averages over
the data. To calculate these nuisance values,
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
uses a technique similar to `weights = "proportional"` in
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

To replicate
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md), we
first create a reference grid to define the factor levels of interest
and the means of continuous variables to condition on.

``` r
grid <- data |>
  mutate(FEV1_BL = mean(FEV1_BL), WEIGHT = mean(WEIGHT)) |>
  distinct(ARMCD, AVISIT, FEV1_BL, WEIGHT) |>
  arrange(ARMCD, AVISIT)
grid
#> # A tibble: 8 × 4
#>   ARMCD AVISIT FEV1_BL WEIGHT
#>   <fct> <fct>    <dbl>  <dbl>
#> 1 PBO   VIS1      40.1  0.519
#> 2 PBO   VIS2      40.1  0.519
#> 3 PBO   VIS3      40.1  0.519
#> 4 PBO   VIS4      40.1  0.519
#> 5 TRT   VIS1      40.1  0.519
#> 6 TRT   VIS2      40.1  0.519
#> 7 TRT   VIS3      40.1  0.519
#> 8 TRT   VIS4      40.1  0.519
```

We use the grid to construct a model matrix with the desired
interactions between continuous variables and factors of interest. Each
column represents a model coefficient, and each row represents a
marginal mean of interest.

``` r
transform <- model.matrix(
  object = ~ FEV1_BL * AVISIT + ARMCD * AVISIT + WEIGHT,
  data = grid
)
rownames(transform) <- paste(grid$ARMCD, grid$AVISIT)
transform
#>          (Intercept)  FEV1_BL AVISITVIS2 AVISITVIS3 AVISITVIS4 ARMCDTRT
#> PBO VIS1           1 40.12532          0          0          0        0
#> PBO VIS2           1 40.12532          1          0          0        0
#> PBO VIS3           1 40.12532          0          1          0        0
#> PBO VIS4           1 40.12532          0          0          1        0
#> TRT VIS1           1 40.12532          0          0          0        1
#> TRT VIS2           1 40.12532          1          0          0        1
#> TRT VIS3           1 40.12532          0          1          0        1
#> TRT VIS4           1 40.12532          0          0          1        1
#>             WEIGHT FEV1_BL:AVISITVIS2 FEV1_BL:AVISITVIS3 FEV1_BL:AVISITVIS4
#> PBO VIS1 0.5185461            0.00000            0.00000            0.00000
#> PBO VIS2 0.5185461           40.12532            0.00000            0.00000
#> PBO VIS3 0.5185461            0.00000           40.12532            0.00000
#> PBO VIS4 0.5185461            0.00000            0.00000           40.12532
#> TRT VIS1 0.5185461            0.00000            0.00000            0.00000
#> TRT VIS2 0.5185461           40.12532            0.00000            0.00000
#> TRT VIS3 0.5185461            0.00000           40.12532            0.00000
#> TRT VIS4 0.5185461            0.00000            0.00000           40.12532
#>          AVISITVIS2:ARMCDTRT AVISITVIS3:ARMCDTRT AVISITVIS4:ARMCDTRT
#> PBO VIS1                   0                   0                   0
#> PBO VIS2                   0                   0                   0
#> PBO VIS3                   0                   0                   0
#> PBO VIS4                   0                   0                   0
#> TRT VIS1                   0                   0                   0
#> TRT VIS2                   1                   0                   0
#> TRT VIS3                   0                   1                   0
#> TRT VIS4                   0                   0                   1
#> attr(,"assign")
#>  [1] 0 1 2 2 2 3 4 5 5 5 6 6 6
#> attr(,"contrasts")
#> attr(,"contrasts")$AVISIT
#> [1] "contr.treatment"
#> 
#> attr(,"contrasts")$ARMCD
#> [1] "contr.treatment"
```

We want to predict at the “average” of `SEX` and `RACE` across all the
data. Since `SEX` and `RACE` are factors, we cannot simply take the
means of the variables themselves. Rather, we construct a model matrix
to turn each factor *level* into a dummy variable, and then average
those dummy variables across the entire dataset. This process accounts
for the observed frequencies of these levels in the data (ideal for
passive variables that the experiment does not directly control), while
guarding against hidden confounding with the factors of interest (which
can lead to Simpson’s paradox).[⁶](#fn6)

``` r
proportional_factors <- data |>
  model.matrix(object = ~ 0 + SEX + RACE) |>
  colMeans() |>
  t()
proportional_factors
#>        SEXMale SEXFemale RACEBlack or African American RACEWhite
#> [1,] 0.4670051 0.5329949                     0.3756345 0.2690355
```

``` r
transform <- transform |>
  bind_cols(proportional_factors) |>
  as.matrix()
transform <- transform[, names(coef(model))]
rownames(transform) <- paste(grid$ARMCD, grid$AVISIT)
transform
#>          (Intercept)  FEV1_BL ARMCDTRT AVISITVIS2 AVISITVIS3 AVISITVIS4
#> PBO VIS1           1 40.12532        0          0          0          0
#> PBO VIS2           1 40.12532        0          1          0          0
#> PBO VIS3           1 40.12532        0          0          1          0
#> PBO VIS4           1 40.12532        0          0          0          1
#> TRT VIS1           1 40.12532        1          0          0          0
#> TRT VIS2           1 40.12532        1          1          0          0
#> TRT VIS3           1 40.12532        1          0          1          0
#> TRT VIS4           1 40.12532        1          0          0          1
#>          RACEBlack or African American RACEWhite SEXFemale    WEIGHT
#> PBO VIS1                     0.3756345 0.2690355 0.5329949 0.5185461
#> PBO VIS2                     0.3756345 0.2690355 0.5329949 0.5185461
#> PBO VIS3                     0.3756345 0.2690355 0.5329949 0.5185461
#> PBO VIS4                     0.3756345 0.2690355 0.5329949 0.5185461
#> TRT VIS1                     0.3756345 0.2690355 0.5329949 0.5185461
#> TRT VIS2                     0.3756345 0.2690355 0.5329949 0.5185461
#> TRT VIS3                     0.3756345 0.2690355 0.5329949 0.5185461
#> TRT VIS4                     0.3756345 0.2690355 0.5329949 0.5185461
#>          FEV1_BL:AVISITVIS2 FEV1_BL:AVISITVIS3 FEV1_BL:AVISITVIS4
#> PBO VIS1            0.00000            0.00000            0.00000
#> PBO VIS2           40.12532            0.00000            0.00000
#> PBO VIS3            0.00000           40.12532            0.00000
#> PBO VIS4            0.00000            0.00000           40.12532
#> TRT VIS1            0.00000            0.00000            0.00000
#> TRT VIS2           40.12532            0.00000            0.00000
#> TRT VIS3            0.00000           40.12532            0.00000
#> TRT VIS4            0.00000            0.00000           40.12532
#>          AVISITVIS2:ARMCDTRT AVISITVIS3:ARMCDTRT AVISITVIS4:ARMCDTRT
#> PBO VIS1                   0                   0                   0
#> PBO VIS2                   0                   0                   0
#> PBO VIS3                   0                   0                   0
#> PBO VIS4                   0                   0                   0
#> TRT VIS1                   0                   0                   0
#> TRT VIS2                   1                   0                   0
#> TRT VIS3                   0                   1                   0
#> TRT VIS4                   0                   0                   1
```

Finally, we use this transformation matrix to map estimated model
coefficients to estimated marginal means.

``` r
marginals_custom <- transform %*% coef(model)
marginals_custom
#>                [,1]
#> PBO VIS1 -4.5998295
#> PBO VIS2 -2.5445943
#> PBO VIS3  0.9841880
#> PBO VIS4  5.6013241
#> TRT VIS1 -1.2858526
#> TRT VIS2  0.8466639
#> TRT VIS3  3.8011416
#> TRT VIS4 10.0521521
```

These results are extremely close to the estimated marginal means from
`emmeans`.

``` r
marginals_emmeans |>
  bind_cols(custom = as.numeric(marginals_custom)) |>
  mutate(difference = custom - emmean)
#> # A tibble: 8 × 5
#>   ARMCD AVISIT emmean custom difference
#>   <fct> <fct>   <dbl>  <dbl>      <dbl>
#> 1 PBO   VIS1   -4.60  -4.60    0       
#> 2 PBO   VIS2   -2.54  -2.54    4.44e-16
#> 3 PBO   VIS3    0.984  0.984  -1.11e-16
#> 4 PBO   VIS4    5.60   5.60   -8.88e-16
#> 5 TRT   VIS1   -1.29  -1.29   -1.11e-15
#> 6 TRT   VIS2    0.847  0.847  -5.55e-16
#> 7 TRT   VIS3    3.80   3.80   -1.33e-15
#> 8 TRT   VIS4   10.1   10.1    -1.78e-15
```

`brms.mmrm` follows the procedure above, but in a Bayesian context. The
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md)
creates the matrix above, and
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md) uses it to
transform posterior draws of `brms` model coefficients into posterior
draws of marginal means. These posterior draws of marginal means then
support estimation of treatment effects (via
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md) and
[`brm_marginal_summaries()`](../reference/brm_marginal_summaries.md))
and posterior probabilities on those treatment effects (via
[`brm_marginal_probabilities()`](../reference/brm_marginal_probabilities.md)).
To fine-tune the marginal mean estimation procedure for niche use cases,
you can modify the transformation returned from
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md) and
then supply it to the `transform` argument of
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md).

## Subgroup analysis

Subgroup analysis raises important questions about how nuisance
variables are averaged, and you as the user are responsible for choosing
the approach that best suits the situation. To illustrate, suppose `SEX`
is a pre-specified subgroup. When estimating marginal means, we now wish
to condition on `"Female"` vs `"Male"` while averaging over `RACE`
across the whole dataset. In `emmeans`, this is similar to how we
calculated `marginals_emmeans` above, but we now move `SEX` from
`nuisance` to `specs`:

``` r
emmeans(
  object = model,
  specs = ~SEX:ARMCD:AVISIT,
  weights = "proportional",
  nuisance = c("USUBJID", "RACE")
)
#>  SEX    ARMCD AVISIT emmean    SE  df lower.CL upper.CL
#>  Male   PBO   VIS1   -5.014 0.752 772   -6.490   -3.538
#>  Female PBO   VIS1   -4.237 0.743 772   -5.696   -2.778
#>  Male   TRT   VIS1   -1.700 0.800 772   -3.270   -0.130
#>  Female TRT   VIS1   -0.923 0.786 772   -2.466    0.620
#>  Male   PBO   VIS2   -2.959 0.752 772   -4.435   -1.482
#>  Female PBO   VIS2   -2.182 0.743 772   -3.640   -0.723
#>  Male   TRT   VIS2    0.433 0.800 772   -1.137    2.003
#>  Female TRT   VIS2    1.209 0.786 772   -0.333    2.752
#>  Male   PBO   VIS3    0.570 0.752 772   -0.906    2.046
#>  Female PBO   VIS3    1.347 0.743 772   -0.112    2.806
#>  Male   TRT   VIS3    3.387 0.800 772    1.816    4.958
#>  Female TRT   VIS3    4.164 0.786 772    2.621    5.707
#>  Male   PBO   VIS4    5.187 0.752 772    3.712    6.663
#>  Female PBO   VIS4    5.964 0.743 772    4.506    7.422
#>  Male   TRT   VIS4    9.638 0.800 772    8.067   11.209
#>  Female TRT   VIS4   10.415 0.786 772    8.872   11.958
#> 
#> Results are averaged over the levels of: 1 nuisance factors 
#> Confidence level used: 0.95
```

This may be reasonable in some cases, and it mitigates the kind of
hidden confounding between the subgroup and other variables which may
otherwise cause Simpson’s paradox. However, for subgroup-specific
marginal means, it may not be realistic to condition on a single point
estimate for all levels of the reference grid. For example, if the model
were to regress on a `pregnancy` variable, then the marginal means for
`SEX = "Male"` should always condition on `pregnancy = 0` instead of
`mean(data$pregnancy)`. And in general, it may be more reasonable to
condition on subgroup-specific averages of nuisance variables. However,
if you do this, it is your responsibility to investigate and understand
the hidden interactions and confounding in your dataset.
<https://cran.r-project.org/package=emmeans/vignettes/interactions.html>
is an edifying vignette on this topic.

To opt into subgroup-specific averages of nuisance variables in
`brms.mmrm`, set `average_within_subgroup = TRUE` in
[`brm_transform_marginal()`](../reference/brm_transform_marginal.md),
then supply the output to the `transform` argument of
[`brm_marginal_draws()`](../reference/brm_marginal_draws.md).

To replicate `brm_transform_marginal(average_within_subgroup = TRUE)`
from scratch, first create a reference grid which includes subgroup
levels.

``` r
grid <- data |>
  distinct(ARMCD, SEX, AVISIT) |>
  arrange(ARMCD, SEX, AVISIT)
grid
#> # A tibble: 16 × 3
#>    ARMCD SEX    AVISIT
#>    <fct> <fct>  <fct> 
#>  1 PBO   Male   VIS1  
#>  2 PBO   Male   VIS2  
#>  3 PBO   Male   VIS3  
#>  4 PBO   Male   VIS4  
#>  5 PBO   Female VIS1  
#>  6 PBO   Female VIS2  
#>  7 PBO   Female VIS3  
#>  8 PBO   Female VIS4  
#>  9 TRT   Male   VIS1  
#> 10 TRT   Male   VIS2  
#> 11 TRT   Male   VIS3  
#> 12 TRT   Male   VIS4  
#> 13 TRT   Female VIS1  
#> 14 TRT   Female VIS2  
#> 15 TRT   Female VIS3  
#> 16 TRT   Female VIS4
```

For each continuous variable, append the corresponding subgroup-specific
averages to the grid.

``` r
means <- data |>
  group_by(SEX) |>
  summarize(FEV1_BL = mean(FEV1_BL), WEIGHT = mean(WEIGHT), .groups = "drop")
grid <- left_join(x = grid, y = means, by = "SEX")
grid
#> # A tibble: 16 × 5
#>    ARMCD SEX    AVISIT FEV1_BL WEIGHT
#>    <fct> <fct>  <fct>    <dbl>  <dbl>
#>  1 PBO   Male   VIS1      40.3  0.516
#>  2 PBO   Male   VIS2      40.3  0.516
#>  3 PBO   Male   VIS3      40.3  0.516
#>  4 PBO   Male   VIS4      40.3  0.516
#>  5 PBO   Female VIS1      39.9  0.521
#>  6 PBO   Female VIS2      39.9  0.521
#>  7 PBO   Female VIS3      39.9  0.521
#>  8 PBO   Female VIS4      39.9  0.521
#>  9 TRT   Male   VIS1      40.3  0.516
#> 10 TRT   Male   VIS2      40.3  0.516
#> 11 TRT   Male   VIS3      40.3  0.516
#> 12 TRT   Male   VIS4      40.3  0.516
#> 13 TRT   Female VIS1      39.9  0.521
#> 14 TRT   Female VIS2      39.9  0.521
#> 15 TRT   Female VIS3      39.9  0.521
#> 16 TRT   Female VIS4      39.9  0.521
```

Begin creating the variable transformation matrix using this new grid.
Be sure to include the subgroup in the formula below exactly as it
appears in the formula used to fit the model.

``` r
transform <- model.matrix(
  object = ~ FEV1_BL * AVISIT + ARMCD * AVISIT + SEX + WEIGHT,
  data = grid
)
```

Append subgroup-specific averages of the levels of nuisance factors (in
this case, just `RACE`).

``` r
proportions <- data |>
  model.matrix(object = ~ 0 + RACE) |>
  as.data.frame() |>
  mutate(SEX = data$SEX) |>
  group_by(SEX) |>
  summarize(across(everything(), mean), .groups = "drop")
transform <- transform |>
  as.data.frame() |>
  mutate(SEX = grid$SEX) |>
  left_join(y = proportions, by = "SEX") |>
  select(-SEX) |>
  as.matrix()
```

Complete the transformation matrix by assigning the correct row names
and aligning the column order with that of the model coefficients.

``` r
rownames(transform) <- paste(grid$ARMCD, grid$SEX, grid$AVISIT)
transform <- transform[, names(coef(model))]
transform
#>                 (Intercept)  FEV1_BL ARMCDTRT AVISITVIS2 AVISITVIS3 AVISITVIS4
#> PBO Male VIS1             1 40.34215        0          0          0          0
#> PBO Male VIS2             1 40.34215        0          1          0          0
#> PBO Male VIS3             1 40.34215        0          0          1          0
#> PBO Male VIS4             1 40.34215        0          0          0          1
#> PBO Female VIS1           1 39.93534        0          0          0          0
#> PBO Female VIS2           1 39.93534        0          1          0          0
#> PBO Female VIS3           1 39.93534        0          0          1          0
#> PBO Female VIS4           1 39.93534        0          0          0          1
#> TRT Male VIS1             1 40.34215        1          0          0          0
#> TRT Male VIS2             1 40.34215        1          1          0          0
#> TRT Male VIS3             1 40.34215        1          0          1          0
#> TRT Male VIS4             1 40.34215        1          0          0          1
#> TRT Female VIS1           1 39.93534        1          0          0          0
#> TRT Female VIS2           1 39.93534        1          1          0          0
#> TRT Female VIS3           1 39.93534        1          0          1          0
#> TRT Female VIS4           1 39.93534        1          0          0          1
#>                 RACEBlack or African American RACEWhite SEXFemale    WEIGHT
#> PBO Male VIS1                       0.4239130 0.2826087         0 0.5161276
#> PBO Male VIS2                       0.4239130 0.2826087         0 0.5161276
#> PBO Male VIS3                       0.4239130 0.2826087         0 0.5161276
#> PBO Male VIS4                       0.4239130 0.2826087         0 0.5161276
#> PBO Female VIS1                     0.3333333 0.2571429         1 0.5206653
#> PBO Female VIS2                     0.3333333 0.2571429         1 0.5206653
#> PBO Female VIS3                     0.3333333 0.2571429         1 0.5206653
#> PBO Female VIS4                     0.3333333 0.2571429         1 0.5206653
#> TRT Male VIS1                       0.4239130 0.2826087         0 0.5161276
#> TRT Male VIS2                       0.4239130 0.2826087         0 0.5161276
#> TRT Male VIS3                       0.4239130 0.2826087         0 0.5161276
#> TRT Male VIS4                       0.4239130 0.2826087         0 0.5161276
#> TRT Female VIS1                     0.3333333 0.2571429         1 0.5206653
#> TRT Female VIS2                     0.3333333 0.2571429         1 0.5206653
#> TRT Female VIS3                     0.3333333 0.2571429         1 0.5206653
#> TRT Female VIS4                     0.3333333 0.2571429         1 0.5206653
#>                 FEV1_BL:AVISITVIS2 FEV1_BL:AVISITVIS3 FEV1_BL:AVISITVIS4
#> PBO Male VIS1              0.00000            0.00000            0.00000
#> PBO Male VIS2             40.34215            0.00000            0.00000
#> PBO Male VIS3              0.00000           40.34215            0.00000
#> PBO Male VIS4              0.00000            0.00000           40.34215
#> PBO Female VIS1            0.00000            0.00000            0.00000
#> PBO Female VIS2           39.93534            0.00000            0.00000
#> PBO Female VIS3            0.00000           39.93534            0.00000
#> PBO Female VIS4            0.00000            0.00000           39.93534
#> TRT Male VIS1              0.00000            0.00000            0.00000
#> TRT Male VIS2             40.34215            0.00000            0.00000
#> TRT Male VIS3              0.00000           40.34215            0.00000
#> TRT Male VIS4              0.00000            0.00000           40.34215
#> TRT Female VIS1            0.00000            0.00000            0.00000
#> TRT Female VIS2           39.93534            0.00000            0.00000
#> TRT Female VIS3            0.00000           39.93534            0.00000
#> TRT Female VIS4            0.00000            0.00000           39.93534
#>                 AVISITVIS2:ARMCDTRT AVISITVIS3:ARMCDTRT AVISITVIS4:ARMCDTRT
#> PBO Male VIS1                     0                   0                   0
#> PBO Male VIS2                     0                   0                   0
#> PBO Male VIS3                     0                   0                   0
#> PBO Male VIS4                     0                   0                   0
#> PBO Female VIS1                   0                   0                   0
#> PBO Female VIS2                   0                   0                   0
#> PBO Female VIS3                   0                   0                   0
#> PBO Female VIS4                   0                   0                   0
#> TRT Male VIS1                     0                   0                   0
#> TRT Male VIS2                     1                   0                   0
#> TRT Male VIS3                     0                   1                   0
#> TRT Male VIS4                     0                   0                   1
#> TRT Female VIS1                   0                   0                   0
#> TRT Female VIS2                   1                   0                   0
#> TRT Female VIS3                   0                   1                   0
#> TRT Female VIS4                   0                   0                   1
```

Finally, use the custom `transform` matrix to estimate subgroup-specific
marginal means. Because we averaged `FEV1_BL`, `WEIGHT`, and `RACE`
within subgroup levels, the results will differ from those of `emmeans`.

``` r
transform %*% coef(model)
#>                       [,1]
#> PBO Male VIS1   -5.0812041
#> PBO Male VIS2   -3.0267423
#> PBO Male VIS3    0.5040980
#> PBO Male VIS4    5.1144252
#> PBO Female VIS1 -4.1780538
#> PBO Female VIS2 -2.1221408
#> PBO Female VIS3  1.4048382
#> PBO Female VIS4  6.0279403
#> TRT Male VIS1   -1.7672272
#> TRT Male VIS2    0.3645159
#> TRT Male VIS3    3.3210517
#> TRT Male VIS4    9.5652532
#> TRT Female VIS1 -0.8640769
#> TRT Female VIS2  1.2691174
#> TRT Female VIS3  4.2217919
#> TRT Female VIS4 10.4787682
```

## References

Bedrick, E. J., Christensen, R., and Johnson, W. (1996), “A new
perspective on priors for generalized linear models,” *Journal of the
American Statistical Association*, 91, 1450–1460.
<https://doi.org/10.2307/2291571>.

Bedrick, E. J., Christensen, R., and Johnson, W. (1997), “Bayesian
binomial regression: Predicting survival at a trauma center,” *Journal
of the American Statistical Association*, 51, 211–218.
<https://doi.org/10.2307/2684890>.

Christensen, R., Johnson, W., Branscum, A., and Hanson, T. E. (2010),
*Bayesian ideas and data analysis*, CRC Press, Taylor; Francis Group, p.
203. <https://doi.org/10.1201/9781439894798>.

Lenth, R. V. (2016), “Least-squares means: The r package lsmeans,”
*Journal of Statistical Software*, 69, 1–33.
<https://doi.org/10.18637/jss.v069.i01>.

Rosner, G. L., Laud, P. W., and Johnson, W. O. (2021), *Bayesian
thinking in biostatistics*, CRC Press, Taylor; Francis Group.
<https://doi.org/10.1201/9781439800102>.

Searle, S. R., Speed, F. M., and Milliken, G. A. (1980), “Population
marginal means in the linear model: An alternative to least squares
means,” *The American Statistician*, 34, 216–221.
<https://doi.org/10.1080/00031305.1980.10483031>.

------------------------------------------------------------------------

1.  For a subgroup analysis where the subgroup factor is identified in
    advance, we would be interested in each combination of treatment
    group, subgroup level, and time point.

2.  Other quantities of interest are downstream of these marginal means.
    For example, the difference between `TRT` and `PBO` at time point
    `VIS4`, and the difference between `VIS4` and `VIS4` for the `TRT`
    group, are simple contrasts of the upstream marginal means.

3.  [`summary()`](https://rdrr.io/r/base/summary.html) also invisibly
    returns a simple character vector with the equations below.

4.  Unlike `emmeans`, `brms.mmrm` completes the grid of patient visits
    to add rows for implicitly missing responses. Completing the grid of
    patient visits ensures all patients are represented equally when
    averaging over baseline covariates, regardless of patients who drop
    out early.

5.  You can then estimate the posterior of any function of marginal
    means by simply applying that function to the individual posterior
    draws from
    [`brm_marginal_draws()`](../reference/brm_marginal_draws.md).
    [`brm_marginal_grid()`](../reference/brm_marginal_grid.md) helps
    identify column names for this kind of custom
    inference/post-processing.

6.  For more context, please refer to the [`emmeans` basics
    vignette](https://cran.r-project.org/package=emmeans/vignettes/basics.html),
    as well as discussion forums
    [here](https://stats.stackexchange.com/questions/332167/what-are-ls-means-useful-for)
    and
    [here](https://stats.stackexchange.com/questions/510862/is-least-squares-means-lsmeans-statistical-nonsense/510923#510923).
