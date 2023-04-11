#' @title Basic MMRM
#' @export
#' @family model
#' @description Fit a basic MMRM model using `brms`.
#' @return A fitted model object from `brms`.
#' @param data A tidy data frame with one row per patient per discrete
#'   time point.
#' @param time Character of length 1, name of the discrete time variable
#'   in the data.
#' @param patient Character of length 1, name of the patient ID
#'   variable in the data.
#' @param formula An R formula to represent the fixed effect parameterization.
#'   Use [brm_formula()] to help.
#' @param correlation Character of length 1, name of the correlation
#'   structure. Only `"unstructured"` is currently supported.
#' @param sd_intercept Positive numeric of length 1, prior standard deviation
#'   of the "intercept" class of parameters.
#' @param sd_b Positive numeric of length 1, prior standard deviation
#'   of the "b" class of parameters.
#' @param sd_intercept_sigma Positive numeric of length 1,
#'   prior standard deviation
#'   of the "intercept" class of parameters with `dpar = "sigma"`.
#' @param sd_b_sigma Positive numeric of length 1,
#'   prior standard deviation
#'   of the "b" class of parameters with `dpar = "sigma"`.
#' @param ... Arguments to `brms::brm()` other than `data`, `formula`,
#'   and `prior`.
#' @examples
#' set.seed(0L)
#' data <- brm_simulate()$data
#' formula <- brm_formula(
#'   response = "response",
#'   group = "group",
#'   time = "time",
#'   effect_base = FALSE,
#'   interaction_base = FALSE
#' )
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = data,
#'         formula = formula,
#'         time = "time",
#'         patient = "patient",
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
  correlation = "unstructured",
  time = "AVISIT",
  patient = "USUBJID",
  sd_intercept = 100,
  sd_b = 100,
  sd_intercept_sigma = 100,
  sd_b_sigma = 100,
  ...
) {
  assert(is.data.frame(data), message = "data arg must be a data frame.")
  assert(
    inherits(formula, "formula"),
    message = "formula arg must be a formula."
  )
  correlations <- "unstructured"
  assert(
    correlation %in% correlations,
    message = paste(
      "correlation arg must be one of:",
      paste(correlations, collapse = ", ")
    )
  )
  assert_chr(time)
  assert_chr(patient)
  assert_pos(sd_intercept)
  assert_pos(sd_b)
  assert_pos(sd_intercept_sigma)
  assert_pos(sd_b_sigma)
  assert(
    time %in% colnames(data),
    message = "time arg must be in colnames(data)."
  )
  assert(
    patient %in% colnames(data),
    message = "patient arg must be in colnames(data)."
  )
  formula_autocor <- as.formula(
    sprintf("~brms::unstr(time = %s, gr = %s)", time, patient)
  )
  formula_sigma <- as.formula(sprintf("sigma ~ %s", time))
  brms_formula <- brms::brmsformula(
    formula = formula,
    autocor = formula_autocor,
    formula_sigma
  )
  prior_0 <- sprintf("normal(0, %s)", sd_intercept)
  prior_b <- sprintf("normal(0, %s)", sd_b)
  prior_0_sigma <- sprintf("normal(0, %s)", sd_intercept_sigma)
  prior_b_sigma <- sprintf("normal(0, %s)", sd_b_sigma)
  brms_prior <- brms::set_prior(prior = prior_0, class = "Intercept") +
    brms::set_prior(prior = prior_b, class = "b") +
    brms::set_prior(prior_0_sigma, class = "Intercept", dpar = "sigma") +
    brms::set_prior(prior_b_sigma, class = "b", dpar = "sigma")
  brms::brm(
    formula = brms_formula,
    prior = brms_prior,
    data = data,
    ...
  )
}
