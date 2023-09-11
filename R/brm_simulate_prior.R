#' @title Prior predictive draws.
#' @export
#' @family simulation
#' @description Simulate the outcome variable from the prior
#'   predictive distribution of an MMRM using `brms`.
#' @details `brm_simulate_prior()` calls `brms::brm()` with
#'   `sample_prior = "only"`, which sets the default intercept prior
#'   using the outcome variable and requires at least some elements of the
#'   outcome variable to be non-missing in advance. So to provide feasible and
#'   consistent output, `brm_simulate_prior()` temporarily sets the
#'   outcome variable to all zeros before invoking `brms::brm()`.
#' @return A list with the following elements:
#'   * `data`: a classed `tibble` with the outcome variable simulated as a draw
#'     from the prior predictive distribution (the final row of `outcome` in
#'     the output). If you simulated a missingness pattern
#'     with [brm_simulate_outline()], then that missingness pattern is applied
#'     so that the appropriate values of the outcome variable are set to `NA`.
#'   * `model`: the `brms` model fit object.
#'   * `model_matrix`: the model matrix of the fixed effects, obtained from
#'     `brms::make_standata()`.
#'   * `outcome`: a numeric matrix with one column per row of `data` and one
#'     row per saved prior predictive draw.
#'   * `parameters`: a `tibble` of saved parameter draws from the prior
#'     predictive distribution.
#' @param prior A valid `brms` prior object with proper priors for parameters
#'   `b` (model coefficients), `b_sigma` (log residual standard deviations
#'   for each time point), and `cortime` (residual correlations among
#'   time points within patients). See the default argument of
#'   [brm_simulate_prior()] for an example. You may have to remove the
#'   `set_prior()` component with `class = "Intercept"` if you set
#'   `intercept = FALSE` in [brm_formula()].
#' @inheritParams brm_model
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_simulate_outline()
#' data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
#' formula <- brm_formula(
#'   data = data,
#'   effect_baseline = FALSE,
#'   interaction_baseline = FALSE
#' )
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       out <- brm_simulate_prior(
#'         data = data,
#'         formula = formula
#'       )
#'     )
#'   )
#' )
#' out$data
#' }
brm_simulate_prior <- function(
  data,
  formula = brms.mmrm::brm_formula(),
  prior = brms::set_prior("student_t(3, 0, 1.3)", class = "Intercept") +
    brms::set_prior("student_t(3, 0, 1.2)", class = "b") +
    brms::set_prior("student_t(3, 0, 1.1)", class = "b", dpar = "sigma") +
    brms::set_prior("lkj(1)", class = "cortime"),
  ...
) {
  brm_data_validate(data = data)
  assert(
    inherits(formula, "brmsformula"),
    message = "formula arg must be a \"brmsformula\" object."
  )
  assert(
    inherits(prior, "brmsprior"),
    message = "prior arg must be a \"brmsprior\" object or NULL."
  )
  data_temp <- data
  outcome <- attr(data_temp, "brm_outcome")
  data_temp[[outcome]] <- 0
  model <- brms::brm(
    data = data_temp,
    formula = formula,
    prior = prior,
    sample_prior = "only",
    ...
  )
  outcome_draws <- stats::predict(object = model, summary = FALSE)
  stan_data <- brms::make_standata(
    formula = formula,
    data = data_temp,
    prior = prior
  )
  data[[outcome]] <- as.numeric(outcome_draws[nrow(outcome_draws), ])
  missing <- attr(data_temp, "brm_missing")
  if (!is.null(missing) && !is.null(data[[missing]])) {
    data[[outcome]][data[[missing]]] <- NA_real_
  }
  list(
    data = data,
    model = model,
    model_matrix = stan_data$X,
    outcome = outcome_draws,
    parameters = posterior::summarize_draws(model)
  )
}
