#' @title Marginal summaries of the data.
#' @export
#' @family marginals
#' @description Marginal summaries of the data.
#' @return A tibble with one row per summary statistic and the following
#'   columns:
#'    * `group`: treatment group.
#'    * `time`: discrete time point.
#'    * `statistic`: type of summary statistic.
#'    * `value`: numeric value of the estimate.
#'   The `statistic` column has the following possible values:
#'    * `mean`: observed mean response after removing missing values.
#'    * `median`: observed median response after removing missing values.
#'    * `sd`: observed standard deviation of the response after
#'      removing missing values.
#'    * `lower`: lower bound of a normal equal-tailed confidence interval
#'      with confidence level determined by the `level` argument.
#'    * `upper`: upper bound of a normal equal-tailed confidence interval
#'      with confidence level determined by the `level` argument.
#'    * `n_observe`: number of non-missing values in the response.
#'    * `n_total`: number of total records in the data for the given
#'      group/time combination, including both observed and missing values.
#' @inheritParams brm_formula
#' @inheritParams brm_model
#' @param level Numeric of length 1 from 0 to 1, level of the confidence
#'   intervals.
#' @examples
#' set.seed(0L)
#'  data <- brm_data(
#'    data = tibble::as_tibble(brm_simulate()$data),
#'    outcome = "response",
#'    role = "response",
#'    group = "group",
#'    time = "time",
#'    patient = "patient"
#'  )
#' data$group <- paste("treatment", data$group)
#' data$time <- paste("visit", data$time)
#' brm_marginal_data(data = data)
brm_marginal_data <- function(data, level = 0.95) {
  brm_data_validate(data)
  assert(level, . >= 0, . <= 1, message = "level arg must be between 0 and 1")
  z <- stats::qnorm(p = (1 - level) / 2)
  data <- tibble::tibble(
    outcome = data[[attr(data, "outcome")]],
    group = data[[attr(data, "group")]],
    time = data[[attr(data, "time")]]
  )
  data <- dplyr::group_by(data, group, time)
  out <- dplyr::summarize(
    .data = data,
    mean = mean(outcome, na.rm = TRUE),
    median = median(outcome, na.rm = TRUE),
    sd = sd(outcome, na.rm = TRUE),
    n_observed = sum(!is.na(outcome)),
    n_total = length(outcome),
    lower = mean - z * sd / sqrt(n_observed),
    upper = mean + z * sd / sqrt(n_observed),
    .groups = "drop"
  )
  out <- tidyr::pivot_longer(
    data = out,
    cols = -tidyselect::any_of(c("group", "time")),
    names_to = "statistic",
    values_to = "value"
  )
  columns <- c("statistic", "group", "time", "value")
  out <- out[, columns]
  args <- lapply(setdiff(columns, "value"), as.symbol)
  args$.data <- out
  do.call(what = dplyr::arrange, args = args)
}
