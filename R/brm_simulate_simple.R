#' @title Simple MMRM simulation.
#' @export
#' @family simulation
#' @description Simple function to simulate a dataset from a simple
#'   specialized MMRM.
#' @details Refer to the methods vignette for a full model specification.
#'   The [brm_simulate_simple()] function simulates a dataset from a
#'   simple pre-defined MMRM. It assumes a cell means structure for fixed
#'   effects, which means there is one fixed effect scalar parameter
#'   (element of vector `beta`) for each unique combination of levels of
#'   treatment group and discrete time point.
#'   The elements of `beta` have independent univariate normal
#'   priors with mean 0 and standard deviation `hyper_beta`.
#'   The residual log standard deviation parameters (elements of vector `tau`)
#'   have normal priors with mean 0 and standard deviation `hyper_tau`.
#'   The residual correlation matrix parameter `lambda` has an LKJ correlation
#'   prior with shape parameter `hyper_lambda`.
#' @return A list of three objects:
#'   * `data`: A tidy dataset with one row per patient per discrete
#'     time point and columns for the outcome and ID variables.
#'   * `model_matrix`: A matrix with one row per row of `data` and columns
#'     that represent levels of the covariates.
#'   * `parameters`: A named list of parameter draws sampled from the prior:
#'     * `beta`: numeric vector of fixed effects.
#'     * `tau`: numeric vector of residual log standard parameters for each
#'       time point.
#'     * `sigma`: numeric vector of residual standard parameters for each
#'       time point. `sigma` is equal to `exp(tau)`.
#'     * `lambda`: correlation matrix of the residuals among the time points
#'       within each patient.
#'     * `covariance`: covariance matrix of the residuals among the time points
#'       within each patient. `covariance` is equal to
#'       `diag(sigma) %*% lambda %*% diag(sigma)`.
#' @param n_group Positive integer of length 1, number of treatment groups.
#' @param n_patient Positive integer of length 1, number of patients
#'   per treatment group.
#' @param n_time Positive integer of length 1, number of discrete
#'   time points (e.g. scheduled study visits) per patient.
#' @param hyper_beta Positive numeric of length 1, hyperparameter.
#'   Prior standard deviation of the fixed effect parameters `beta`.
#' @param hyper_tau Positive numeric of length 1, hyperparameter.
#'   Prior standard deviation parameter of the residual log standard
#'   deviation parameters `tau`
#' @param hyper_lambda Positive numeric of length 1, hyperparameter.
#'   Prior shape parameter of the LKJ correlation matrix of the residuals
#'   among discrete time points.
#' @examples
#' set.seed(0L)
#' simulation <- brm_simulate_simple()
#' simulation$data
brm_simulate_simple <- function(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  hyper_beta = 1,
  hyper_tau = 0.1,
  hyper_lambda = 1
) {
  assert_pos(n_group, message = "n_group must be 1 positive number")
  assert_pos(n_patient, message = "n_patient must be 1 positive number")
  assert_pos(n_time, message = "n_time must be 1 positive number")
  assert_pos(hyper_beta, message = "hyper_beta must be 1 positive number")
  assert_pos(hyper_tau, message = "hyper_tau must be 1 positive number")
  assert_pos(hyper_lambda, message = "hyper_lambda must be 1 positive number")
  patients <- tibble::tibble(
    group = paste0("group_", rep(seq_len(n_group), each = n_patient)),
    patient = paste0("patient_", seq_len(n_group * n_patient))
  )
  levels_time <- paste0("time_", seq_len(n_time))
  grid <- tidyr::expand_grid(patients, time = levels_time)
  model_matrix <- model.matrix(~ 0 + group + time, data = grid)
  beta <- stats::rnorm(n = ncol(model_matrix), mean = 0, sd = hyper_beta)
  mean <- as.numeric(model_matrix %*% beta)
  tau <- stats::rnorm(n = n_time, mean = 0, sd = hyper_tau)
  sigma <- exp(tau)
  lambda <- trialr::rlkjcorr(n = 1L, K = n_time, eta = hyper_lambda)
  covariance <- diag(sigma) %*% lambda %*% diag(sigma)
  response <- tapply(
    X = mean,
    INDEX = as.factor(grid$patient),
    FUN = MASS::mvrnorm,
    n = 1L,
    Sigma = covariance
  )
  data <- tibble::tibble(response = do.call(what = c, args = response), grid)
  data <- brm_data(
    data = data,
    outcome = "response",
    role = "response",
    baseline = NULL,
    group = "group",
    time = "time",
    patient = "patient",
    covariates = character(0L),
    missing = NULL,
    level_control = "group_1",
    level_baseline = "time_1"
  )
  parameters <- list(
    beta = beta,
    tau = tau,
    sigma = sigma,
    lambda = lambda,
    covariance = covariance
  )
  list(data = data, model_matrix = model_matrix, parameters = parameters)
}
