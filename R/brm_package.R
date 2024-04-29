#' brms.mmrm: Bayesian MMRMs using `brms`
#' @name brms.mmrm-package
#' @description The mixed model for repeated measures (MMRM) is a
#'   popular model for longitudinal clinical trial data with
#'   continuous endpoints, and `brms` a is powerful and versatile
#'   package for fitting Bayesian regression models.
#'   The `brms.mmrm` R package leverages `brms` to run MMRMs, and
#'   it supports a simplified interfaced to reduce difficulty
#'   and align with the best practices of the life sciences.
#' @references
#'   * Bürkner, P.-C. (2017), "brms: An R package for Bayesian
#'     multilevel models using Stan,"
#'     Journal of Statistical Software, 80, 1–28.
#'     https://doi.org/10.18637/jss.v080.i01.
#'   * Mallinckrodt, C. H., Lane, P. W., Schnell, D., and others (2008),
#'     "Recommendations for the primary analysis of continuous endpoints
#'     in longitudinal clinical trials,"
#'     Therapeutic Innovation and Regulatory Science, 42, 303–319.
#'     https://doi.org/10.1177/009286150804200402.
#'   * Mallinckrodt, C. H., and Lipkovich, I. (2017),
#'     Analyzing longitudinal clinical trial data: A practical guide,
#'     CRC Press, Taylor & Francis Group.
#' @family help
#' @importFrom brms brm brmsformula get_prior make_standata prior unstr
#' @importFrom dplyr across bind_cols bind_rows left_join rename select
#'   summarize
#' @importFrom ggplot2 aes facet_wrap geom_point geom_errorbar ggplot
#'   position_dodge theme_gray xlab ylab
#' @importFrom ggridges geom_density_ridges2
#' @importFrom MASS mvrnorm
#' @importFrom Matrix rankMatrix
#' @importFrom posterior as_draws_df mcse_mean mcse_median mcse_quantile
#'   mcse_sd
#' @importFrom purrr map_dbl map_df map2_df
#' @importFrom rlang is_formula warn
#' @importFrom stats as.formula median model.matrix rbinom rnorm runif sd
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr expand_grid pivot_longer pivot_wider
#' @importFrom tidyselect any_of everything starts_with
#' @importFrom trialr rlkjcorr
#' @importFrom utils capture.output globalVariables head
#' @importFrom zoo na.locf
NULL

globalVariables(
  c(
    ".",
    "b",
    "correlations",
    "Intercept",
    "normal",
    "sigma",
    "n_observed",
    "lower",
    "upper",
    "source",
    "group",
    "time",
    "value",
    "outcome",
    "level",
    "patient"
  ),
  package = "brms.mmrm"
)
