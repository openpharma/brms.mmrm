#' brms.mmrm: Bayesian MMRMs using `brms`
#' @docType package
#' @name brms.mmrm-package
#' @description The mixed model for repeated measures (MMRM) is a
#'   popular model for longitudinal clinical trial data with
#'   continuous endpoints, and `brms` is powerful and versatile
#'   package for fitting Bayesian regression models.
#'   The `brms.mmrm` R package leverages `brms` to run MMRMs, and
#'   it supports a simplified interfaced to reduce difficulty
#'   and align with best practices for the life sciences.
#' @references
#'   * Paul-Christian Bürkner (2017). brms: An R Package for Bayesian
#'     Multilevel Models Using Stan.
#'.    Journal of Statistical Software, 80(1), 1-28.
#'     doi:10.18637/jss.v080.i01
#'   * Mallinckrodt, C.H., Lane, P.W., Schnell, D. et al.
#'     Recommendations for the Primary Analysis of Continuous Endpoints
#'     in Longitudinal Clinical Trials.
#'     Ther Innov Regul Sci 42, 303–319 (2008).
#'     doi:10.1177/009286150804200402
#' @family help
#' @importFrom brms brm brmsformula get_prior prior unstr
#' @importFrom coda as.mcmc
#' @importFrom dplyr bind_rows left_join
#' @importFrom emmeans emmeans
#' @importFrom ggplot2 aes facet_wrap geom_point geom_errorbar ggplot
#'   position_dodge theme_gray xlab ylab
#' @importFrom MASS mvrnorm
#' @importFrom posterior as_draws_df mcse_mean mcse_median mcse_quantile
#'   mcse_sd
#' @importFrom purrr map_dbl map_df map2_df
#' @importFrom rlang warn
#' @importFrom stats as.formula median model.matrix rnorm runif sd
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid pivot_longer pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom trialr rlkjcorr
#' @importFrom utils capture.output globalVariables
NULL

globalVariables(
  c(
    ".",
    "b",
    "Intercept",
    "normal",
    "sigma",
    "n_observed",
    "lower",
    "upper",
    "source",
    "group",
    "time"
  ),
  package = "brms.mmrm"
)
