#' @title Marginal probabilities on the treatment effect for an MMRM.
#' @export
#' @family marginals
#' @description Marginal probabilities on the treatment effect for an MMRM.
#' @return A tibble of probabilities of the form
#'   `Prob(treatment effect > threshold | data)` and/or
#'   `Prob(treatment effect < threshold | data)`. It has one row per
#'   probability and the following columns:
#'     * `group`: treatment group.
#'     * `time`: discrete time point,
#'     * `direction`: direction of the comparison in the marginal probability:
#'       `"greater"` for `>`, `"less"` for `<`
#'     * `threshold`: treatment effect threshold in the probability statement.
#'     * `value`: numeric value of the estimate of the probability.
#' @inheritParams brm_marginal_summaries
#' @param direction Character vector of the same length as `threshold`.
#'   `"greater"` to compute the marginal posterior probability that the
#'   treatment effect is greater than the threshold,
#'   `"less"` to compute the marginal posterior probability that the
#'   treatment effect is less than the threshold.
#'   Each element `direction[i]` corresponds to `threshold[i]`
#'   for all `i` from 1 to `length(direction)`.
#' @param threshold Numeric vector of the same length as `direction`,
#'   treatment effect threshold for computing posterior probabilities.
#'   Each element `direction[i]` corresponds to `threshold[i]` for
#'   all `i` from 1 to `length(direction)`.
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
#' formula <- brm_formula(
#'   data = data,
#'   baseline = FALSE,
#'   baseline_time = FALSE
#' )
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = data,
#'         formula = formula,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' draws <- brm_marginal_draws(model = model, data = data)
#' brm_marginal_probabilities(draws, direction = "greater", threshold = 0)
#' }
brm_marginal_probabilities <- function(
  draws,
  direction = "greater",
  threshold = 0
) {
  assert(
    is.list(draws),
    message = "draws arg must be a named list from brm_marginal_draws()"
  )
  assert(
    direction,
    is.character(.),
    !anyNA(.),
    nzchar(.),
    . %in% c("greater", "less"),
    message = "elements of the direction arg must be \"greater\" or \"less\""
  )
  assert(
    threshold,
    is.numeric(.),
    is.finite(.),
    message = "threshold arg must be a numeric vector"
  )
  assert(
    length(direction) == length(threshold),
    message = "direction and threshold must have the same length"
  )
  draws <- tibble::as_tibble(draws$difference_group)
  for (name in names_mcmc) {
    draws[[name]] <- NULL
  }
  out <- purrr::map2_df(
    .x = direction,
    .y = threshold,
    .f = ~summarize_probabilities(
      draws = draws,
      direction = .x,
      threshold = .y
    )
  )
  columns <- c("direction", "threshold", "group", "time", "value")
  out <- out[, columns]
  args <- lapply(setdiff(columns, "value"), as.symbol)
  args$.data <- out
  do.call(what = dplyr::arrange, args = args)
}

summarize_probabilities <- function(draws, direction, threshold) {
  values <- purrr::map_dbl(
    draws,
    ~marginal_probability(.x, direction, threshold)
  )
  out <- tibble::tibble(
    group = names_group(draws),
    time = names_time(draws),
    direction = direction,
    threshold = threshold,
    value = values
  )
  out <- unname_df(out)
}

marginal_probability <- function(difference, direction, threshold) {
  if_any(
    direction == "greater",
    mean(difference > threshold),
    mean(difference < threshold)
  )
}
