#' @title Model formula
#' @export
#' @family model
#' @description Build a model formula for an MMRM.
#' @return An object of class `"brmsformula"` returned from
#'   `brms::brmsformula()`. It contains the fixed effect parameterization,
#'   correlation structure, and residual variance structure.
#' @param response Character of length 1, name of the response variable
#'   in the data.
#' @param group Character of length 1, name of the treatment group
#'   variable in the data.
#' @param base Character of length 1, name of the baseline response variable
#'   in the data.
#' @param time Character of length 1, name of the discrete time variable
#'   in the data.
#' @param patient Character of length 1, name of the patient ID variable
#'   in the data.
#' @param covariates Character vector of names of other covariates
#'   in the data.
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
#' brm_formula()
#' brm_formula(intercept = FALSE, effect_base = FALSE)
#' brm_formula(
#'   intercept = FALSE,
#'   effect_base = FALSE,
#'   interaction_group = FALSE
#' )
brm_formula <- function(
  response = "CHG",
  base = "BASE",
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0),
  intercept = TRUE,
  effect_base = TRUE,
  effect_group = TRUE,
  effect_time = TRUE,
  interaction_base = TRUE,
  interaction_group = TRUE,
  correlation = "unstructured"
) {
  assert_chr(response, "response arg must be a nonempty character string")
  assert_chr(base, "base arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_chr(patient, "patient arg must be a nonempty character string")
  assert_chr_vec(covariates, "covariates arg must be a nonempty chr vector")
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
  terms <- c(
    term("0", !intercept),
    term(time, effect_time),
    term(base, effect_base),
    term(paste0(base, ":", time), interaction_base),
    term(group, effect_group),
    term(paste0(group, ":", time), interaction_group),
    covariates,
    term_correlation(correlation, time, patient)
  )
  right <- paste(terms, collapse = " + ")
  formula <- stats::as.formula(paste(response, "~", right))
  formula_sigma <- stats::as.formula(paste("sigma ~", time))
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
