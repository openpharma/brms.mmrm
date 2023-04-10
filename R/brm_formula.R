#' @title Model formula
#' @export
#' @description Build a model formula for an MMRM.
#' @return A formula to specify an MMRM.
#' @param response Character of length 1, name of the response variable.
#' @param base Character of length 1, name of the baseline response variable.
#' @param time Character of length 1, name of the discrete time variable.
#' @param group Character of length 1, name of the treatment group
#'   variable.
#' @param covariates Character vector of names of other covariates.
#' @param effect_group `TRUE` to include an additive effect for treatment
#'   group, `FALSE` to omit.
#' @param effect_time `TRUE` to include an additive effect for discrete
#'   time, `FALSE` to omit.
#' @param intercept `TRUE` to include an intercept, `FALSE` to omit.
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
  group = "TRT01P",
  time = "AVISIT",
  base = "BASE",
  covariates = character(0),
  intercept = TRUE,
  effect_group = TRUE,
  effect_time = TRUE,
  effect_base = TRUE,
  interaction_base = TRUE,
  interaction_group = TRUE
) {
  assert_chr(response, "response arg must be a nonempty character string")
  assert_chr(base, "base arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr_vec(covariates, "covariates arg must be a nonempty chr vector")
  assert_lgl(intercept)
  assert_lgl(effect_group)
  assert_lgl(effect_time)
  assert_lgl(effect_base)
  assert_lgl(interaction_base)
  assert_lgl(interaction_group)
  terms <- c(
    term("0", !intercept),
    term(time, effect_time),
    term(base, effect_base),
    term(paste0(base, ":", time), interaction_base),
    term(group, effect_group),
    term(paste0(group, ":", time), interaction_group)
  )
  right <- paste(terms, collapse = " + ")
  paste(response, "~", right)
}

term <- function(name, condition) {
  if_any(condition, name, character(0L))
}
