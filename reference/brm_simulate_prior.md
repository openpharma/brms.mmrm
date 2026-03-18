# Prior predictive draws.

Simulate the outcome variable from the prior predictive distribution of
an MMRM using `brms`.

## Usage

``` r
brm_simulate_prior(
  data,
  formula,
  prior = brms.mmrm::brm_prior_simple(data = data, formula = formula),
  ...
)
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

- prior:

  A valid `brms` prior object with proper priors for parameters `b`
  (model coefficients), `b_sigma` (log residual standard deviations for
  each time point), and `cortime` (residual correlations among time
  points within patients). See the
  [`brm_prior_simple()`](brm_prior_simple.md) function for an example.

- ...:

  Named arguments to specific [`brm_formula()`](brm_formula.md) methods.

## Value

A list with the following elements:

- `data`: a classed `tibble` with the outcome variable simulated as a
  draw from the prior predictive distribution (the final row of
  `outcome` in the output). If you simulated a missingness pattern with
  [`brm_simulate_outline()`](brm_simulate_outline.md), then that
  missingness pattern is applied so that the appropriate values of the
  outcome variable are set to `NA`.

- `model`: the `brms` model fit object.

- `model_matrix`: the model matrix of the fixed effects, obtained from
  [`brms::make_standata()`](https://paulbuerkner.com/brms/reference/standata.html).

- `outcome`: a numeric matrix with one column per row of `data` and one
  row per saved prior predictive draw.

- `parameters`: a `tibble` of saved parameter draws from the prior
  predictive distribution.

## Details

`brm_simulate_prior()` calls
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) with
`sample_prior = "only"`, which sets the default intercept prior using
the outcome variable and requires at least some elements of the outcome
variable to be non-missing in advance. So to provide feasible and
consistent output, `brm_simulate_prior()` temporarily sets the outcome
variable to all zeros before invoking
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## See also

Other simulation:
[`brm_simulate_categorical()`](brm_simulate_categorical.md),
[`brm_simulate_continuous()`](brm_simulate_continuous.md),
[`brm_simulate_outline()`](brm_simulate_outline.md),
[`brm_simulate_simple()`](brm_simulate_simple.md)

## Examples

``` r
if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
set.seed(0L)
data <- brm_simulate_outline()
data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
data$response <- rnorm(nrow(data))
formula <- brm_formula(
  data = data,
  baseline = FALSE,
  baseline_time = FALSE
)
tmp <- utils::capture.output(
  suppressMessages(
    suppressWarnings(
      out <- brm_simulate_prior(
        data = data,
        formula = formula
      )
    )
  )
)
out$data
}
```
