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
#' sim <- brm_simulate()
#' data <- sim$data
#' data$group <- paste("treatment", data$group)
#' data$time <- paste("visit", data$time)
#' brm_marginal_data(
#'   data = data,
#'   response = "response",
#'   group = "group",
#'   time = "time"
#' )
brm_marginal_data <- function(
  data,
  response = "CHG",
  group = "TRT01P",
  time = "AVISIT",
  level = 0.95
) {
  assert_chr(response, "response arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_num(level, "level arg must be a length-1 numeric between 0 and 1")
  for (field in c(response, group, time)) {
    assert(
      field %in% colnames(data),
      message = sprintf("\"%s\" is not a column in the data.", field)
    )
  }
  assert(level, . >= 0, . <= 1, message = "level arg must be between 0 and 1")
  z <- stats::qnorm(p = (1 - level) / 2)
  data <- tibble::tibble(
    response = data[[response]],
    group = data[[group]],
    time = data[[time]]
  )
  data <- dplyr::group_by(data, group, time)
  out <- dplyr::summarize(
    .data = data,
    mean = mean(response, na.rm = TRUE),
    median = median(response, na.rm = TRUE),
    sd = sd(response, na.rm = TRUE),
    n_observed = sum(!is.na(response)),
    n_total = length(response),
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