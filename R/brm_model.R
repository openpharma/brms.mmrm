#' @title Basic MMRM
#' @export
#' @family models
#' @description Fit a basic MMRM model using `brms`.
#' @return A fitted model object from `brms`.
#' @param data A tidy data frame with one row per patient per discrete
#'   time point.
#' @param formula An object of class `"brmsformula"` from [brm_formula()]
#'   or `brms::brmsformula()`. Should include the full parameterization
#'   of the model, including fixed effects, residual correlation,
#'   and heterogeneity in the discrete-time-specific residual variance
#'   components.
#' @param prior Either `NULL` or a `"brmsprior"` object from `brms::prior()`.
#' @param ... Arguments to `brms::brm()` other than `data`, `formula`,
#'   and `prior`.
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_data(
#'   data = tibble::as_tibble(brm_simulate()$data),
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient"
#' )
#' formula <- brm_formula(
#'   data = data,
#'   effect_base = FALSE,
#'   interaction_base = FALSE
#' )
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = data,
#'         formula = formula,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' model
#' brms::prior_summary(model)
#' }
brm_model <- function(
  data,
  formula = brms.mmrm::brm_formula(),
  prior = brms::prior("lkj_corr_cholesky(1)", class = "Lcortime"),
  ...
) {
  brm_data_validate(data = data)
  assert(
    inherits(formula, "brmsformula"),
    message = "formula arg must be a \"brmsformula\" object."
  )
  if (!is.null(prior)) {
    assert(
      inherits(prior, "brmsprior"),
      message = "prior arg must be a \"brmsprior\" object or NULL."
    )
  }
  brms::brm(
    data = data,
    formula = formula,
    prior = prior,
    ...
  )
}
