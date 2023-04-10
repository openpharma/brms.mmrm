#' @title Model formula
#' @export
#' @description Build a model formula for an MMRM.
#' @return A formula to specify an MMRM.
#' @param response Character of length 1, name of the response variable.
#' @param base Character of length 1, name of the baseline response variable.
#'   `NULL` to omit.
#' @param time Character of length 1, name of the discrete time variable.
#'   `NULL` to omit.
#' @param group Character of length 1, name of the treatment group
#'   variable.
#'   `NULL` to omit.
#' @param covariates Character vector of names of other covariates.
#' @param intercept `TRUE` to include an intercept, `FALSE` to omit.
#' @param interaction_base `TRUE` to include baseline-by-time interaction,
#'   `FALSE` to omit.
#' @param interaction_group `TRUE` to include treatment-group-by-time
#'   interaction, `FALSE` to omit.
#' @examples
#' brm_formula()
#' brm_formula(intercept = FALSE, base = NULL)
#' brm_formula(intercept = FALSE, base = NULL, interaction_group = FALSE)
brm_formula <- function(
  response = "CHG",
  base = "BASE",
  time = "AVISIT",
  group = "TRT01P",
  covariates = character(0),
  intercept = TRUE,
  interaction_base = TRUE,
  interaction_group = TRUE
) {
  assert_flag(response, "response arg must be a nonempty character string")
  assert_flag(base, "base arg must be a nonempty character string")
  assert_flag(time, "time arg must be a nonempty character string")
  assert_flag(group, "group arg must be a nonempty character string")
  assert_chr_vec(covariates, "covariates arg must be a nonempty chr vector")
  assert_lgl(intercept)
  assert_lgl(interaction_base)
  assert_lgl(interaction_group)
  
}