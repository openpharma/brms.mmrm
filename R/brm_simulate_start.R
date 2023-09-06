#' @title Start a simulated dataset
#' @export
#' @family simulation
#' @description Begin creating a simulated dataset.
#' @return A data frame from [brm_data()] with attributes to define roles
#'   for various columns in the dataset. The data frame has one row per
#'   patient per time point and the following columns:
#'   * `group`: integer index of the treatment group.
#'   * `patient`: integer index of the patient.
#'   * `time`: integer index of the discrete time point.
#' @param n_group Positive integer of length 1, number of treatment groups.
#' @param n_patient Positive integer of length 1, number of patients
#'   per treatment group.
#' @param n_time Positive integer of length 1, number of discrete
#'   time points (e.g. scheduled study visits) per patient.
#' @examples
#' brm_simulate_start()
brm_simulate_start <- function(n_group = 2L, n_patient = 100L, n_time = 4L) {
  assert_pos(n_group, message = "n_group must be 1 positive number")
  assert_pos(n_patient, message = "n_patient must be 1 positive number")
  assert_pos(n_time, message = "n_time must be 1 positive number")
  patients <- tibble::tibble(
    group = rep(seq_len(n_group), each = n_patient),
    patient = seq_len(n_group * n_patient)
  )
  tidyr::expand_grid(patients, time = seq_len(n_time))
}
