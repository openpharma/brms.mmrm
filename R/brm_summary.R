#' @title Summarize an MMRM.
#' @export
#' @family results
#' @description Summarize a basic MMRM model fit.
#' @return A named list of two data frames with one row per summary
#'   statistic:
#'   * `means`: summary statistics of the marginal posterior mean of
#'     each treatment group and time point. Columns include `marginal`
#'     for the type of marginal distribution (see the "Value" section of
#'     the [brm_marginals()] help file for details), `group` for treatment
#'     group, `time` for discrete time point, `statistic` for the type of
#'     summary statistic, `value` for the numeric value of the estimate,
#'     and `mcse` for the Monte Carlo standard error of the estimate.
#'   * `probabilities`: marginal posterior probabilities of the form
#'     `Prob(treatment effect > threshold | data)` and/or
#'     `Prob(treatment effect < threshold | data)`. Columns include
#'     `group` for the treatment group, `time` for the discrete time point,
#'     `direction` to indicate the direction of the comparison
#'     (`"greater"` for `>`, `"less"` for `<`), `threshold` for the
#'     treatment effect threshold in the probability statement,
#'     and `value` for the numberic value of the estimate of the
#'     probability.
#' @param marginals Posterior draws of the marginal posterior
#'   obtained from [brm_marginals()].
#' @param level Numeric of length 1 between 0 and 1, credible level
#'   for the credible intervals. Only relevant when `return = "marginals"`.
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
#' set.seed(0L)
#' sim <- brm_simulate()
#' data <- sim$data
#' data$group <- paste("treatment", data$group)
#' data$time <- paste("visit", data$time)
#' formula <- brm_formula(
#'   response = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   effect_base = FALSE,
#'   interaction_base = FALSE
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
#' marginals <- brm_marginals(
#'   model = model,
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   control = "treatment 1",
#'   baseline = "visit 1",
#'   outcome = "response"
#' )
#' brm_summary(marginals)
brm_summary <- function(
  marginals,
  level = 0.95,
  direction = "greater",
  threshold = 0
) {
  assert(
    is.list(marginals),
    message = "marginals arg must be a named list from brm_marginals()"
  )
  assert_num(level, "level arg must be a length-1 numeric between 0 and 1")
  assert(level, . >= 0, . <= 1, message = "level arg must be between 0 and 1")
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
  table_response <- summarize_marginals(marginals$response, level)
  table_change <- if_any(
    "change" %in% names(marginals),
    summarize_marginals(marginals$change, level),
    NULL
  )
  table_difference <- summarize_marginals(marginals$difference, level)
  means <- dplyr::bind_rows(
    response = table_response,
    change = table_change,
    difference = table_difference,
    .id = "marginal"
  )
  probabilities <- summarize_probabilities(
    draws = marginals$difference,
    direction = direction,
    threshold = threshold
  )
  list(means = means, probabilities = probabilities)
}

summarize_marginals <- function(draws, level) {
  level_lower <- (1 - level) / 2
  level_upper <- 1 - level_lower
  draws[[".chain"]] <- NULL
  draws[[".iteration"]] <- NULL
  draws[[".draw"]] <- NULL
  value <- tibble::tibble(
    group = names_group(draws),
    time = names_time(draws),
    mean = purrr::map_dbl(draws, mean),
    median = purrr::map_dbl(draws, median),
    sd = purrr::map_dbl(draws, sd),
    lower = purrr::map_dbl(draws, ~quantile(.x, level_lower)),
    upper = purrr::map_dbl(draws, ~quantile(.x, level_upper))
  )
  mcse <- tibble::tibble(
    group = names_group(draws),
    time = names_time(draws),
    mean = purrr::map_dbl(draws, posterior::mcse_mean),
    median = purrr::map_dbl(draws, posterior::mcse_median),
    sd = purrr::map_dbl(draws, posterior::mcse_sd),
    lower = purrr::map_dbl(draws, ~posterior::mcse_quantile(.x, level_lower)),
    upper = purrr::map_dbl(draws, ~posterior::mcse_quantile(.x, level_upper))
  )
  value <- tidyr::pivot_longer(
    data = value,
    cols = -any_of(c("group", "time")),
    names_to = "statistic",
    values_to = "value"
  )
  mcse <- tidyr::pivot_longer(
    data = mcse,
    cols = -any_of(c("group", "time")),
    names_to = "statistic",
    values_to = "mcse"
  )
  dplyr::left_join(
    x = value,
    y = mcse,
    by = c("group", "time", "statistic")
  )
}

summarize_probabilities <- function(draws, direction, threshold) {
  draws[[".chain"]] <- NULL
  draws[[".iteration"]] <- NULL
  draws[[".draw"]] <- NULL
  tibble::tibble(
    group = names_group(draws),
    time = names_time(draws),
    direction = direction,
    theshold = threshold,
    value = purrr::map_dbl(
      draws,
      ~marginal_probability(.x, direction, threshold)
    )
  )
}

marginal_probability <- function(difference, direction, threshold) {
  if_any(
    direction == "greater",
    mean(difference > threshold),
    mean(difference < threshold)
  )
}
