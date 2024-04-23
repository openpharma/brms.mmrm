#' @title Simple prior for a `brms` MMRM
#' @export
#' @family priors
#' @description Generate a simple prior for a `brms` MMRM.
#' @details In [brm_prior_simple()], you can separately choose priors for
#'   the intercept, model coefficients, log-scale standard deviations,
#'   and pairwise correlations between time points within patients.
#'   However, each class of parameters is set as a whole. In other words,
#'   [brm_prior_simple()] cannot assign different priors
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
#' @param correlation Deprecated on 2024-04-22
#'   (version 0.1.0.9004). Please use arguments like `"unstructured"`,
#'   and/or `"autoregressive"` to supply correlation-specific priors.
#' @param unstructured Character of length 1,
#'   Stan code for an unstructured correlation prior.
#'   Supply the empty string `""` to set a flat prior (default).
#'   Applies to the `"cortime` parameter class in `brms` priors.
#'   Used for formulas created with
#'   `brm_formula(correlation = "unstructured")`. LKJ is recommended.
#'   See also [brms::unstr()].
#' @param autoregressive Character of length 1,
#'   Stan code for a prior on autoregressive correlation parameters.
#'   Supply the empty string `""` to set a flat prior (default).
#'   Applies to the `"ar` parameter class in `brms` priors.
#'   Used for formulas created with
#'   `brm_formula(correlation = "autoregressive")` and
#'   `brm_formula(correlation = "autoregressive_moving_average")`.
#'   See also [brms::ar()] and [brms::arma()].
#' @param moving_average Character of length 1,
#'   Stan code for a prior on moving average correlation parameters.
#'   Supply the empty string `""` to set a flat prior (default).
#'   Applies to the `"ma` parameter class in `brms` priors.
#'   Used for formulas created with
#'   `brm_formula(correlation = "moving_average")` and
#'   `brm_formula(correlation = "autoregressive_moving_average")`.
#'   See also [brms::ma()] and [brms::arma()].
#' @param compound_symmetry Character of length 1,
#'   Stan code for a prior on compound symmetry correlation parameters.
#'   Supply the empty string `""` to set a flat prior (default).
#'   Applies to the `"cosy` parameter class in `brms` priors.
#'   Used for formulas created with
#'   `brm_formula(correlation = "compound_symmetry")`.
#'   See also [brms::cosy()].
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
#'   unstructured = "lkj(2.5)"
#' )
brm_prior_simple <- function(
  data,
  formula,
  intercept = "student_t(3, 0, 2.5)",
  coefficients = "student_t(3, 0, 2.5)",
  sigma = "student_t(3, 0, 2.5)",
  unstructured = "lkj(1)",
  autoregressive = "",
  moving_average = "",
  compound_symmetry = "",
  correlation = NULL
) {
  brm_data_validate(data = data)
  brm_formula_validate(formula)
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
    sigma,
    message = "'sigma' must be a valid character string"
  )
  assert_chr(
    paste(unstructured, "x"),
    message = "'unstructured' must be a valid character string"
  )
  assert_chr(
    paste(autoregressive, "x"),
    message = "'autoregressive' must be a valid character string"
  )
  assert_chr(
    paste(moving_average, "x"),
    message = "'moving_average' must be a valid character string"
  )
  assert_chr(
    paste(compound_symmetry, "x"),
    message = "'compound_symmetry' must be a valid character string"
  )
  if (!is.null(correlation)) {
    brm_deprecate(
      "The correlation argumument of brm_prior_simple() ",
      "was deprecated on 2024-04-22 (version 0.1.0.9004). ",
      "Please use arguments like \"unstructured\", ",
      "and \"autoregressive\" to supply correlation-specific priors."
    )
  }
  data[[attr(data, "brm_outcome")]] <- 0
  prior <- brms::get_prior(formula = formula, data = data)
  prior$prior[prior$class == "Intercept"] <- intercept
  prior$prior[prior$class == "b" & prior$dpar == ""] <- coefficients
  prior$prior[prior$dpar == "sigma"] <- sigma
  correlation_classes <- c("cortime", "ar", "ma")
  prior$prior[prior$class == "cortime"] <- unstructured
  prior$prior[prior$class == "ar"] <- autoregressive
  prior$prior[prior$class == "ma"] <- moving_average
  prior$prior[prior$class == "cosy"] <- compound_symmetry
  prior[prior$coef == "", ]
}
