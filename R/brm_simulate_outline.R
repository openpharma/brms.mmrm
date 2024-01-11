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
#' @param rate_dropout Numeric of length 1 between 0 and 1,
#'   post-baseline dropout rate.
#'   A dropout is an intercurrent event when data
#'   collection for a patient stops permanently,
#'   causing the outcomes for that patient to be missing during and after
#'   the dropout occurred. The first time point is assumed to be baseline,
#'   so dropout is there. Dropouts are equally likely to occur at each of
#'   the post-baseline time points.
#' @param rate_lapse Numeric of length 1, expected proportion of post-baseline
#'   outcomes that are missing. Missing outcomes of this type are independent
#'   and uniformly distributed across the data.
#' @examples
#' brm_simulate_outline()
brm_simulate_outline <- function(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  rate_dropout = 0.1,
  rate_lapse = 0.05
) {
  data <- brm_simulate_grid(n_group, n_patient, n_time)
  data <- brm_simulate_dropout(data = data, rate = rate_dropout)
  data <- brm_simulate_lapse(data = data, rate = rate_lapse)
  data <- brm_simulate_levels(data = data)
  data$response <- NA_real_
  data <- brm_data(
    data = data,
    outcome = "response",
    role = "response",
    baseline = NULL,
    group = "group",
    time = "time",
    patient = "patient",
    covariates = character(0L),
    missing = "missing",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  brm_data_validate(data)
  data
}

brm_simulate_grid <- function(n_group, n_patient, n_time) {
  assert_pos(n_group, message = "n_group must be 1 positive number")
  assert_pos(n_patient, message = "n_patient must be 1 positive number")
  assert_pos(n_time, message = "n_time must be 1 positive number")
  patients <- tibble::tibble(
    group = rep(seq_len(n_group), each = n_patient),
    patient = seq_len(n_group * n_patient)
  )
  data <- tidyr::expand_grid(patients, time = seq_len(n_time))
  data$missing <- FALSE
  data
}

brm_simulate_dropout <- function(data, rate) {
  assert(
    rate,
    length(.) == 1L,
    is.numeric(.),
    !anyNA(.),
    . >= 0,
    . <= 1,
    message = "rate_dropout must be a single valid proportion."
  )
  rows_baseline <- dplyr::filter(data, time == min(time))
  data <- dplyr::filter(data, time > min(time))
  data <- dplyr::arrange(data, group, patient, time)
  data <- dplyr::group_by(data, patient)
  n_time <- length(unique(data$time))
  data <- dplyr::mutate(
    .data = data,
    missing = missing | brm_dropout_patient(rate, n_time)
  )
  data <- dplyr::ungroup(data)
  data <- dplyr::bind_rows(rows_baseline, data)
  dplyr::arrange(data, group, patient, time)
}

brm_dropout_patient <- function(rate, n_time) {
  if_any(
    as.logical(stats::rbinom(n = 1L, size = 1L, prob = rate)),
    seq_len(n_time) >= sample.int(n = n_time, size = 1L),
    rep(FALSE, n_time)
  )
}

brm_simulate_lapse <- function(data, rate) {
  assert(
    rate,
    length(.) == 1L,
    is.numeric(.),
    !anyNA(.),
    . >= 0,
    . <= 1,
    message = "rate_lapse must be a single valid proportion."
  )
  index <- data$time > min(data$time)
  n <- sum(index)
  missing <- as.logical(stats::rbinom(n = n, size = 1L, prob = rate))
  data$missing[index] <- data$missing[index] | missing
  data
}

brm_simulate_levels <- function(data) {
  for (field in c("group", "patient", "time")) {
    data[[field]] <- paste0(field, "_", data[[field]])
  }
  data
}
