#' @title Basic MMRM
#' @export
#' @family model
#' @description Fit a basic MMRM model using `brms`.
#' @return A fitted model object from `brms`.
#' @param data A tidy data frame with one row per patient per discrete
#'   time point.
#' @param formula An object of class `"brmsformula"` from [brm_formula()]
#'   or `brms::brmsformula()`. Should include the full parameterization
#'   of the model, including fixed effects, residual correlation,
#'   and heterogeneity in the discrete-time-specific residual variance
#'   components.
#' @param sd_intercept Positive numeric of length 1, prior standard deviation
#'   of the "intercept" class of parameters.
#' @param sd_b Positive numeric of length 1, prior standard deviation
#'   of the "b" class of parameters.
#' @param sd_sigma Positive numeric of length 1,
#'   prior standard deviation
#'   of the "b" class of parameters with `dpar = "sigma"`.
#' @param shape_cor Positive numeric of length 1. For unstructured
#'   correlation, this is the LKJ shape parameter.
#' @param ... Arguments to `brms::brm()` other than `data`, `formula`,
#'   and `prior`.
#' @examples
#' set.seed(0L)
#' sim <- brm_simulate()
#' data <- sim$data
#' formula <- brm_formula(
#'   response = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
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
brm_model <- function(
  data,
  formula = brm_formula(),
  sd_intercept = 100,
  sd_b = 100,
  sd_sigma = 100,
  shape_cor = 1,
  ...
) {
  assert(is.data.frame(data), message = "data arg must be a data frame.")
  assert(
    inherits(formula, "brmsformula"),
    message = "formula arg must be a \"brmsformula\" object."
  )
  assert_pos(sd_intercept)
  assert_pos(sd_b)
  assert_pos(sd_sigma)
  prior_0 <- sprintf("normal(0, %s)", sd_intercept)
  prior_b <- sprintf("normal(0, %s)", sd_b)
  prior_sigma <- sprintf("normal(0, %s)", sd_sigma)
  prior_cor <- sprintf("lkj_corr_cholesky(%s)", shape_cor)
  prior <- brms::set_prior(prior = prior_b, class = "b") +
    brms::set_prior(prior_sigma, class = "b", dpar = "sigma") +
    brms::set_prior(prior_cor, class = "Lcortime")
  prior_classes <- brms::get_prior(formula = formula, data = data)$class
  if ("Intercept" %in% prior_classes) {
    prior <- prior + brms::set_prior(prior = prior_0, class = "Intercept")
  }
  brms::brm(data = data, formula = formula, prior = prior, ...)
}
