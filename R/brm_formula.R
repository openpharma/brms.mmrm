#' @title Model formula
#' @export
#' @family models
#' @description Build a model formula for an MMRM.
#' @return An object of class `"brmsformula"` returned from
#'   `brms::brmsformula()`. It contains the fixed effect parameterization,
#'   correlation structure, and residual variance structure.
#' @param data A classed data frame from [brm_data()].
#' @param correlation Character of length 1, name of the correlation
#'   structure. Only `"unstructured"` is currently supported.
#' @param intercept `TRUE` to include an intercept, `FALSE` to omit.
#' @param effect_base `TRUE` to include an additive effect for baseline
#'   response, `FALSE` to omit.
#' @param effect_group `TRUE` to include an additive effects for treatment
#'   groups, `FALSE` to omit.
#' @param effect_time `TRUE` to include a additive effect for discrete
#'   time points, `FALSE` to omit.
#' @param interaction_base `TRUE` to include baseline-by-time interaction,
#'   `FALSE` to omit.
#' @param interaction_group `TRUE` to include treatment-group-by-time
#'   interaction, `FALSE` to omit.
#' @examples
#' set.seed(0)
#' data <- brm_data(
#'   data = brm_simulate()$data,
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient"
#' )
#' brm_formula(data)
#' brm_formula(data = data, intercept = FALSE, effect_base = FALSE)
#' brm_formula(
#'   data = data,
#'   intercept = FALSE,
#'   effect_base = FALSE,
#'   interaction_group = FALSE
#' )
brm_formula <- function(
  data,
  intercept = TRUE,
  effect_base = TRUE,
  effect_group = TRUE,
  effect_time = TRUE,
  interaction_base = TRUE,
  interaction_group = TRUE,
  correlation = "unstructured"
) {
  brm_data_validate(data)
  assert_lgl(intercept)
  assert_lgl(effect_group)
  assert_lgl(effect_time)
  assert_lgl(effect_base)
  assert_lgl(interaction_base)
  assert_lgl(interaction_group)
  assert_chr(
    correlation,
    "correlation arg must be a nonempty character string"
  )
  correlations <- "unstructured"
  assert(
    correlation %in% correlations,
    message = paste(
      "correlation arg must be one of:",
      paste(correlations, collapse = ", ")
    )
  )
  outcome <- attr(data, "brm_outcome")
  role <- attr(data, "brm_role")
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  terms <- c(
    term("0", !intercept),
    term(time, effect_time),
    term(base, effect_base && !is.null(base)),
    term(paste0(base, ":", time), interaction_base && !is.null(base)),
    term(group, effect_group),
    term(paste0(group, ":", time), interaction_group),
    covariates,
    term_correlation(correlation, time, patient)
  )
  right <- paste(terms, collapse = " + ")
  formula <- stats::as.formula(paste(outcome, "~", right))
  formula_sigma <- stats::as.formula(paste("sigma ~ 0 +", time))
  brms::brmsformula(formula = formula, formula_sigma)
}

term <- function(name, condition) {
  if_any(condition, name, character(0L))
}

term_correlation <- function(correlation, time, patient) {
  switch(
    correlation,
    unstructured = sprintf("unstr(time = %s, gr = %s)", time, patient)
  )
}
