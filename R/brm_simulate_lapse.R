
brm_simulate_lapse <- function(data, rate = 0.025) {
  brm_data_validate(data)
  rows_baseline <- dplyr::filter(data, time == 1L)
  rows_baseline$miss_lapse <- FALSE
  data <- dplyr::filter(data, time > 1L)
  miss_lapse <- stats::rbinom(n = nrow(data), size = 1L, prob = rate)
  data$miss_lapse <- as.logical(miss_lapse)
  data <- dplyr::bind_rows(rows_baseline, data)
  dplyr::arrange(data, group, patient, time)
}


brm_simulate_dropout <- function(out, rate) {
  rows_baseline <- dplyr::filter(out, time == 1L)
  rows_baseline$miss_dropout <- FALSE
  out <- dplyr::filter(out, time > 1L)
  out <- dplyr::arrange(out, group, patient, time)
  out <- dplyr::group_by(out, patient)
  out <- dplyr::mutate(
    .data = out,
    miss_dropout = brm_dropout_patient(rate_dropout, n_time - baseline)
  )
  out <- dplyr::ungroup(out)
  miss_lapse <- stats::rbinom(n = nrow(out), size = 1L, prob = rate_lapse)
  out$miss_lapse <- as.logical(miss_lapse)
  out <- dplyr::bind_rows(rows_baseline, out)
  dplyr::arrange(out, group, patient, time)
}

brm_dropout_patient <- function(rate_dropout, n_time) {
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
