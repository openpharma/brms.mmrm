#' @title Cell-means-like successive differences parameterization
#' @export
#' @family parameterizations
#' @description Create a parameterization on successive differences
#'   between adjacent time points, with an independent set of fixed effects
#'   for each treatment group.
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
#' @section Nuisance variables in parameterizations:
#'   In the presence of other covariates, functions like
#'   [brm_parameterize_successive_cells()] center continuous variables at
#'   their means and center nuisance factors at their proportional
#'   averages in the data. This ensures that the main model coefficients
#'   of interest are not implicitly conditional on a subset of the data.
#'   This is important for effectively utilizing informative priors
#'   on those coefficients of interest.
brm_prameterize_successive_cells <- function(
  data,
  formula
) {
  
}
