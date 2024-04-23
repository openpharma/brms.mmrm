#' @title Successive differences scenario
#' @export
#' @family informative prior scenarios
#' @description Create an informative prior scenario where the fixed effects
#'   are successive differences between adjacent time points.
#' @details In this parameterization, each fixed effect is either an intercept
#'   on the first time point or the difference between two adjacent time
#'   points, and each treatment group has its own set of fixed effects
#'   independent of the other treatment groups.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates.
#'   Let `mu_gt` be the conditional expectation of the response at group
#'   `g` time `t` given data and hyperparameters. Then, the cell-means-like
#'   successive differences parameterization declares model coefficients
#'   `beta_1`, `beta_2`, ... `beta_6` as follows:
#'
#'       `mu_A1 = beta_1`
#'       `mu_A2 = beta_1 + beta_2`
#'       `mu_A3 = beta_1 + beta_2 + beta_3`
#'
#'       `mu_B1 = beta_4`
#'       `mu_B2 = beta_4 + beta_5`
#'       `mu_B3 = beta_4 + beta_5 + beta_6`
#'
#'   For group A, `beta_1` is the time 1 intercept, `beta_2` represents
#'   time 2 minus time 1, and `beta_3` represents time 3 minus time 2.
#'   `beta_4`, `beta_5`, and `beta_6` represent the analogous roles.
#' @section Nuisance variables in informative prior scenarios:
#'   In the presence of other covariates, functions like
#'   [brm_scenario_successive()] convert nuisance factors into binary
#'   dummy variables, then center all those dummy variables and any
#'   continuous nuisance variables at their means in the data.
#'   This ensures that the main model coefficients
#'   of interest are not implicitly conditional on a subset of the data.
#'   In other words, preprocessing nuisance variables this way preserves
#'   the interpretations of the fixed effects of interest, and it ensures
#'   informative priors can be specified correctly.
#' @return A special classed `tibble` with pre-processed data tailored to
#'   the successive differences scenario.
#' @inheritParams brm_model
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
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
#' data
#' brm_scenario_successive(data)
#' }
brm_scenario_successive <- function(data) {
  brm_data_validate(data)
  
}
