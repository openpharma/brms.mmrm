#' @title Model formula
#' @export
#' @family models
#' @description Build a model formula for an MMRM.
#' @section Parameterization:
#'   The formula is not the only factor
#'   that determines the fixed effect parameterization.
#'   The ordering of the categorical variables in the data,
#'   as well as the `contrast` option in R, affect the
#'   construction of the model matrix. To see the model
#'   matrix that will ultimately be used in [brm_model()],
#'   run [brms::make_standata()] and examine the `X` element
#'   of the returned list. See the examples below for a
#'   demonstration.
#' @return An object of class `"brmsformula"` returned from
#'   `brms::brmsformula()`. It contains the fixed effect parameterization,
#'   correlation structure, and residual variance structure.
#' @param data A classed data frame from [brm_data()].
#' @param correlation Character of length 1, name of the correlation
#'   structure. Only `"unstructured"` is currently supported.
#' @param intercept Logical of length 1.
#'   `TRUE` to include an intercept, `FALSE` to omit.
#' @param baseline Logical of length 1.
#'   `TRUE` to include an additive effect for baseline
#'   response, `FALSE` to omit.
#' @param group Logical of length 1.
#'   `TRUE` to include additive effects for treatment groups, `FALSE` to omit.
#' @param time Logical of length 1.
#'   `TRUE` to include a additive effect for discrete time, `FALSE` to omit.
#' @param baseline_time Logical of length 1.
#'   `TRUE` to include baseline-by-time interaction, `FALSE` to omit.
#' @param group_time Logical of length 1.
#'   `TRUE` to include group-by-time interaction, `FALSE` to omit.
#' @param effect_baseline Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `baseline` instead.
#' @param effect_group Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `group` instead.
#' @param effect_time Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `time` instead.
#' @param interaction_baseline Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `baseline_time` instead.
#' @param interaction_group Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `group_time` instead.
#' @examples
#' set.seed(0)
#' data <- brm_data(
#'   data = brm_simulate_simple()$data,
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
#' brm_formula(data)
#' brm_formula(data = data, intercept = FALSE, baseline = FALSE)
#' formula <- brm_formula(
#'   data = data,
#'   intercept = FALSE,
#'   baseline = FALSE,
#'   group = FALSE
#' )
#' formula
#' # Optional: set the contrast option, which determines the model matrix.
#' options(contrasts = c(unordered = "contr.SAS", ordered = "contr.poly"))
#' # See the fixed effect parameterization you get from the data:
#' head(brms::make_standata(formula = formula, data = data)$X)
#' # Specify a different contrast method to use an alternative
#' # parameterization when fitting the model with brm_model():
#' options(
#'   contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")
#' )
#' # different model matrix than before:
#' head(brms::make_standata(formula = formula, data = data)$X)
brm_formula <- function(
  data,
  intercept = TRUE,
  baseline = !is.null(attr(data, "brm_baseline")),
  group = TRUE,
  time = TRUE,
  baseline_time = !is.null(attr(data, "brm_baseline")),
  group_time = TRUE,
  correlation = "unstructured",
  effect_baseline = NULL,
  effect_group = NULL,
  effect_time = NULL,
  interaction_baseline = NULL,
  interaction_group = NULL
) {
  brm_data_validate(data)
  assert_lgl(intercept)
  assert_lgl(group)
  assert_lgl(time)
  assert_lgl(baseline)
  assert_lgl(baseline_time)
  assert_lgl(group_time)
  message <- paste0(
    "%s was deprecated on 2024-01-16 (version 0.0.2.9002).",
    "Use %s instead."
  )
  if (!is.null(effect_baseline)) {
    brm_deprecate(sprintf(message, "effect_baseline", "baseline"))
    baseline <- effect_baseline
  }
  if (!is.null(effect_group)) {
    brm_deprecate(sprintf(message, "effect_group", "group"))
    group <- effect_group
  }
  if (!is.null(effect_time)) {
    brm_deprecate(sprintf(message, "effect_time", "time"))
    time <- effect_time
  }
  if (!is.null(interaction_baseline)) {
    brm_deprecate(sprintf(message, "interaction_baseline", "baseline_time"))
    baseline_time <- interaction_baseline
  }
  if (!is.null(interaction_group)) {
    brm_deprecate(sprintf(message, "interaction_group", "group_time"))
    group_time <- interaction_group
  }
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
  label_outcome <- attr(data, "brm_outcome")
  label_role <- attr(data, "brm_role")
  label_baseline <- attr(data, "brm_baseline")
  label_group <- attr(data, "brm_group")
  label_time <- attr(data, "brm_time")
  label_patient <- attr(data, "brm_patient")
  label_covariates <- attr(data, "brm_covariates")
  terms <- c(
    term("0", !intercept),
    term(label_baseline, baseline),
    term(label_time, time),
    term(c(label_baseline, label_time), baseline_time),
    term(label_group, group),
    term(c(label_group, label_time), group_time),
    label_covariates,
    term_correlation(correlation, label_time, label_patient)
  )
  right <- paste(terms, collapse = " + ")
  formula <- stats::as.formula(paste(label_outcome, "~", right))
  formula_sigma <- stats::as.formula(paste("sigma ~ 0 +", label_time))
  brms::brmsformula(formula = formula, formula_sigma)
}

term <- function(labels, condition) {
  if_any(condition, paste0(labels, collapse = ":"), character(0L))
}

term_correlation <- function(correlation, label_time, label_patient) {
  switch(
    correlation,
    unstructured = sprintf(
      "unstr(time = %s, gr = %s)",
      label_time,
      label_patient
    )
  )
}
