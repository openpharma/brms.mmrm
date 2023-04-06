#' @title Simulate an MMRM.
#' @export
#' @family simulation
#' @description Simulate a dataset from a basic MMRM.
#' @details First, a sample of parameters is drawn from the prior
#'   distribution. Then, a set of data is drawn from the data model
#'   conditional on the sample of parameters.
#'   In this initial implementation, the simulation is simple. The only
#'   covariates are treatment group and discrete time,
#'   there are no dropouts, and the
#'   parameterization is not the same as that of `brms`.
#' @return A list of three objects:
#'   * `data`: A tidy dataset with one row per patient per discrete
#'     time point and columns for the response and covariates.
#'   * `model_matrix`: A matrix with one row per row of `data` and columns
#'     that represent levels of the covariates.
#'   * `parameters`: A named list of parameter values sampled from the prior.
#' @param n_group Positive integer of length 1, number of treatment groups.
#' @param n_patient Positive integer of length 1, number of patients
#'   per treatment group.
#' @param n_time Positive integer of length 1, number of discrete
#'   time points (e.g. scheduled study visits) per patient.
#' @param hyper_beta Positive numeric of length 1, hyperparameter.
#'   Prior standard deviation of the fixed effect parameters.
#' @param hyper_sigma Positive numeric of length 1, hyperparameter.
#'   Uniform prior upper bound of the time-specific residual
#'   standard deviation parameters.
#' @param hyper_correlation Positive numeric of length 1, hyperparameter.
#'   LKJ shape parameter of the correlation matrix among repeated
#'   measures within each patient.
#' @examples
#' simulation <- brm_simulate()
#' simulation$data
brm_simulate <- function(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  hyper_beta = 1,
  hyper_sigma = 1,
  hyper_correlation = 1
) {
  assert_pos(n_group, message = "n_group must be 1 positive number")
  assert_pos(n_patient, message = "n_patient must be 1 positive number")
  assert_pos(n_time, message = "n_time must be 1 positive number")
  assert_pos(hyper_beta, message = "hyper_beta must be 1 positive number")
  assert_pos(hyper_sigma, message = "hyper_sigma must be 1 positive number")
  assert_pos(
    hyper_correlation,
    message = "hyper_correlation must be 1 positive number"
  )
  patients <- tibble::tibble(
    group = rep(seq_len(n_group), each = n_patient),
    patient = seq_len(n_group * n_patient)
  )
  grid <- tidyr::expand_grid(patients, time = seq_len(n_time))
  model_matrix <- model.matrix(~ 0 + group + time, data = grid)
  beta <- stats::rnorm(n = ncol(model_matrix), mean = 0, sd = hyper_beta)
  mean <- as.numeric(model_matrix %*% beta)
  sigma <- stats::runif(n = n_time, min = 0, max = hyper_sigma)
  correlation <- trialr::rlkjcorr(n = 1L, K = n_time, eta = hyper_correlation)
  covariance <- diag(sigma) %*% correlation %*% diag(sigma)
  response <- tapply(
    X = mean,
    INDEX = as.factor(grid$patient),
    FUN = MASS::mvrnorm,
    n = 1L,
    Sigma = covariance
  )
  data <- tibble::tibble(response = do.call(what = c, args = response), grid)
  parameters <- list(
    beta = beta,
    sigma = sigma,
    covariance = covariance
  )
  list(data = data, model_matrix = model_matrix, parameters = parameters)
}
