#' @title Simple prior for a `brms` MMRM
#' @export
#' @family priors
#' @description Generate a simple prior for a `brms` MMRM.
#' @details In [brm_prior_simple()], you can separately choose priors for
#'   the intercept, model coefficents, log-scale standard deviations,
#'   and pairwise correlations between time points within patients.
#'   However, each class of parameters is set as a whole. In other words,
#'   [brms_prior_simple()] cannot assign different priors
#'   to different fixed effect parameters.
#' @return A classed data frame with the `brms` prior.
#' @inheritParams brm_model
#' @param intercept Character of length 1, Stan code for the prior
#'   to set on the intercept parameter.
#' @param coefficients Character of length 1, Stan code for the prior
#'   to set independently on each of the non-intercept model coefficients.
#' @param sigma Character of length 1, Stan code for the prior
#'   to set independently on each of the log-scale standard deviation
#'   parameters. Should be a symmetric prior in most situations.
#' @param correlation Character of length 1, Stan code for the prior
#'   on the correlation matrix for the residuals of a given patient.
#'   (Different patients are modeled as independent, and each
#'   patient has the same correlation structure as each other patient.)
#'   Should be an LKJ prior in most situations.
#' @examples
#' set.seed(0L)
#' data <- brm_simulate_outline()
#' data <- brm_simulate_continuous(data, names = c("age", "biomarker"))
#' formula <- brm_formula(
#'   data = data,
#'   baseline = FALSE,
#'   baseline_time = FALSE
#' )
#' brm_prior_simple(
#'   data = data,
#'   formula = formula,
#'   intercept = "student_t(3, 0, 2.5)",
#'   coefficients = "normal(0, 10)",
#'   sigma = "student_t(2, 0, 4)",
#'   correlation = "lkj(2.5)"
#' )
brm_prior_simple <- function(
  data,
  formula,
  intercept = "student_t(3, 0, 2.5)",
  coefficients = "student_t(3, 0, 2.5)",
  sigma = "student_t(3, 0, 2.5)",
  correlation = "lkj(1)"
) {
  brm_data_validate(data = data)
  assert(
    inherits(formula, "brmsformula"),
    message = "formula arg must be a \"brmsformula\" object."
  )
  assert_chr(
    intercept,
    message = "'intercept' must be a valid character string"
  )
  assert_chr(
    coefficients,
    message = "'coefficients' must be a valid character string"
  )
  assert_chr(
    sigma,
    message = "'sigma' must be a valid character string"
  )
  assert_chr(
    correlation,
    message = "'correlation' must be a valid character string"
  )
  data[[attr(data, "brm_outcome")]] <- 0
  prior <- brms::get_prior(formula = formula, data = data)
  prior$prior[prior$class == "Intercept"] <- intercept
  prior$prior[prior$class == "b" & prior$dpar == ""] <- coefficients
  prior$prior[prior$dpar == "sigma"] <- sigma
  prior$prior[prior$class == "cortime"] <- correlation
  prior[prior$coef == "", ]
}
