#' @title Simulate a dataset outline.
#' @export
#' @family simulation
#' @description Simulate an outline of an MMRM dataset.
#' @details This function simulates an outline of an MMRM dataset,
#'   including covariates and a missingness pattern for the response.
#'   However, it does not include the response variable. Simulating the
#'   response from the prior predictive distribution requires a
#'   fixed effect parameterization and a prior distribution, both of which
#'   are outside the scope of [brm_simulate_outline()].
#' @return A data frame from [brm_data()] with attributes to define roles
#'   for various columns in the dataset.
#' @param n_group Positive integer of length 1, number of treatment groups.
#' @param n_patient Positive integer of length 1, number of patients
#'   per treatment group.
#' @param n_time Positive integer of length 1, number of discrete
#'   time points (e.g. scheduled study visits) per patient.
#' @param baseline Logical of length 1, whether the earliest time point
#'   should be considered baseline. If `TRUE`, then the earliest time point
#'   has no missing responses indicated in the `miss_dropout` or `miss_lapse`
#'   columns of the output. Defaults to `FALSE` because `brms.mmrm` assumes
#'   the outcome variable is change from baseline.
#' @param rate_dropout Positive numeric from 0 to 1,
#'   expected proportion of observed dropouts at the final time point.
#'   A dropout at time point `T` is an intercurrent event in which a patient
#'   has missing responses at `T` and all time points after `T`.
#'   For [brm_simulate_outline()], the conditional probability of dropout
#'   at time `T`, given that dropout did not occur at time `T` - 1, is the same
#'   for all time points `T`.
#'   The expected dropout rate is the same for each group.
#'   The resulting missingness pattern is in the
#'   `miss_dropout` column of the output:
#'   `TRUE` if missing, `FALSE` if observed.
#' @param rate_lapse Positive numeric from 0 to 1,
#'   expected proportion of observations
#'   that are missing uniformly at random.
#'   This source of missingness covers isolated temporary lapses
#'   in data collection.
#'   The resulting missingness pattern is in the
#'   `miss_lapse` column of the output:
#'   `TRUE` if missing, `FALSE` if observed.
#' @examples
#' set.seed(0L)
#' brm_simulate_outline()
brm_simulate_outline <- function(
  n_group = 2L,
  n_patient = 100L,
  n_time = 4L,
  baseline = FALSE,
  rate_dropout = 0.1,
  rate_lapse = 0.025,
  ...
) {
  assert_pos(n_group, message = "n_group must be 1 positive number")
  assert_pos(n_patient, message = "n_patient must be 1 positive number")
  assert_pos(n_time, message = "n_time must be 1 positive number")
  out <- brm_simulate_grid(n_group, n_patient, n_time)
  out <- brm_simulate_miss(out, n_time, baseline, rate_dropout, rate_lapse)
  out <- brm_simulate_levels(out)
  out
}

brm_simulate_grid <- function(n_group, n_patient, n_time) {
  patients <- tibble::tibble(
    group = rep(seq_len(n_group), each = n_patient),
    patient = seq_len(n_group * n_patient)
  )
  tidyr::expand_grid(patients, time = seq_len(n_time))
}

brm_simulate_miss <- function(
  out,
  n_time,
  baseline,
  rate_dropout,
  rate_lapse
) {
  if (baseline) {
    rows_baseline <- dplyr::filter(out, time == 1L)
    rows_baseline$miss_lapse <- FALSE
    rows_baseline$miss_dropout <- FALSE
    out <- dplyr::filter(out, time > 1L)
  }
  out <- dplyr::arrange(out, group, patient, time)
  out <- dplyr::group_by(out, patient)
  out <- dplyr::mutate(
    .data = out,
    miss_dropout = brm_simulate_dropout(rate_dropout, n_time - baseline)
  )
  out <- dplyr::ungroup(out)
  miss_lapse <- stats::rbinom(n = nrow(out), size = 1L, prob = rate_lapse)
  out$miss_lapse <- as.logical(miss_lapse)
  if (baseline) {
    out <- dplyr::bind_rows(rows_baseline, out)
    out <- dplyr::arrange(out, group, patient, time)
  }
  out
}

brm_simulate_dropout <- function(rate_dropout, n_time) {
  if_any(
    as.logical(rbinom(n = 1L, size = 1L, prob = rate_dropout)),
    seq_len(n_time) >= sample.int(n = n_time, size = 1L),
    rep(FALSE, n_time)
  )
}

brm_simulate_levels <- function(out) {
  for (field in c("group", "patient", "time")) {
    out[[field]] <- paste(field, out[[field]])
  }
  out
}
