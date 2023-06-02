#' @title Summarize an MMRM.
#' @export
#' @family results
#' @description Summarize a basic MMRM model fit.
#' @details Currently assumes the response variable is `CHG`
#'   (change from baseline) and not `AVAL` (raw response).
#' @return A list of two data frames, one with summary statistics on the
#'   marginal posterior and another with posterior probabilities
#'   on the treatment effects.
#' @inheritParams brm_formula
#' @param model Fitted `brms` model object from [brm_model()].
#' @param outcome Character of length 1, `"response"` if the
#'   response variable is the raw outcome variable (such as AVAL)
#'   or `"change"` if the response variable is change from baseline
#'   (e.g. CHG).
#' @param control Element of the `group` column in the data which indicates
#'   the control group for the purposes of calculating treatment differences.
#' @param baseline Element of the `time` column in the data
#'   which indicates the baseline time for the purposes of calculating
#'   change from baseline.
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
#' brm_summary(
#'   model = model,
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   control = "treatment 1",
#'   baseline = "visit 1",
#'   outcome = "response"
#' )
brm_summary <- function(
  model,
  base = "BASE",
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0),
  outcome = "change",
  control = "Placebo",
  baseline = "Baseline",
  level = 0.95,
  direction = "greater",
  threshold = 0
) {
  assert_chr(base, "base arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_chr(patient, "patient arg must be a nonempty character string")
  assert_chr(
    outcome,
    "outcome arg must be a nonempty character string"
  )
  assert(
    outcome %in% c("response", "change"),
    message = "outcome must be either \"response\" or \"change\""
  )
  assert_chr_vec(covariates, "covariates arg must be a character vector")
  assert(
    control,
    is.atomic(.),
    length(.) == 1L,
    !anyNA(.),
    message = "control arg must be a length-1 non-missing atomic value"
  )
  assert(
    baseline,
    is.atomic(.),
    length(.) == 1L,
    !anyNA(.),
    message = "baseline arg must be a length-1 non-missing atomic value"
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
  assert(is.data.frame(model$data))
  data <- model$data
  assert(
    group %in% colnames(data),
    message = "group arg must be a data column name"
  )
  assert(
    time %in% colnames(data),
    message = "time arg must be a data column name"
  )
  assert(
    patient %in% colnames(data),
    message = "patient arg must be a data column name"
  )
  assert(
    covariates %in% colnames(data),
    message = "all covariates must be data column names"
  )
  assert(
    control %in% data[[group]],
    message = "control arg must be in data[[group]]"
  )
  nuisance <- c(base, patient, covariates)
  emmeans <- emmeans::emmeans(
    object = model,
    specs = as.formula(sprintf("~%s:%s", group, time)),
    weights = "proportional",
    nuisance = nuisance
  )
  draws_response <- posterior::as_draws_df(as.mcmc(emmeans))
  .chain <- draws_response[[".chain"]]
  .iteration <- draws_response[[".iteration"]]
  .draw <- draws_response[[".draw"]]
  draws_response[[".chain"]] <- NULL
  draws_response[[".iteration"]] <- NULL
  draws_response[[".draw"]] <- NULL
  colnames(draws_response) <- gsub(
    pattern = sprintf("^%s ", group),
    replacement = "",
    x = colnames(draws_response)
  )
  colnames(draws_response) <- gsub(
    pattern = sprintf(", %s ", time),
    replacement = ", ",
    x = colnames(draws_response)
  )
  groups <- unique(group_names(draws_response))
  times <- unique(time_names(draws_response))
  draws_response[[".chain"]] <- .chain
  draws_response[[".iteration"]] <- .iteration
  draws_response[[".draw"]] <- .draw
  control <- as.character(control)
  time <- as.character(time)
  assert(
    control %in% groups,
    message = sprintf(
      "control argument \"%s\" is not in one of the treatment groups: %s",
      control,
      paste(groups, collapse = ", ")
    )
  )
  if (outcome == "response") {
    assert(
      baseline %in% times,
      message = sprintf(
        "baseline argument \"%s\" is not in one of the time points: %s",
        baseline,
        paste(times, collapse = ", ")
      )
    )
  }
  if (outcome == "response") {
    draws_change <- subtract_baseline(
      draws = draws_response,
      groups = groups,
      times = times,
      baseline = baseline
    )
    draws_difference <- subtract_control(
      draws = draws_change,
      groups = groups,
      times = setdiff(times, baseline),
      control = control
    )
  } else {
    draws_difference <- subtract_control(
      draws = draws_response,
      groups = groups,
      times = times,
      control = control
    )
  }
  table_response <- summarize_marginals(draws_response, level)
  table_change <- if_any(
    outcome == "response",
    summarize_marginals(draws_change, level),
    NULL
  )
  table_difference <- summarize_marginals(draws_difference, level)
  marginals <- dplyr::bind_rows(
    response = table_response,
    change = table_change,
    difference = table_difference,
    .id = "marginal"
  )
  probabilities <- summarize_probabilities(
    draws = draws_difference,
    direction = direction,
    threshold = threshold
  )
  list(marginals = marginals, probabilities = probabilities)
}

subtract_baseline <- function(draws, groups, times, baseline) {
  out <- draws[, c(".chain", ".iteration", ".draw")]
  for (group in groups) {
    for (time in setdiff(times, baseline)) {
      name1 <- marginal_name(group, baseline)
      name2 <- marginal_name(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

subtract_control <- function(draws, groups, times, control) {
  out <- draws[, c(".chain", ".iteration", ".draw")]
  for (group in setdiff(groups, control)) {
    for (time in times) {
      name1 <- marginal_name(control, time)
      name2 <- marginal_name(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

summarize_marginals <- function(draws, level) {
  level_lower <- (1 - level) / 2
  level_upper <- 1 - level_lower
  draws[[".chain"]] <- NULL
  draws[[".iteration"]] <- NULL
  draws[[".draw"]] <- NULL
  value <- tibble::tibble(
    group = group_names(draws),
    time = time_names(draws),
    mean = purrr::map_dbl(draws, mean),
    median = purrr::map_dbl(draws, median),
    sd = purrr::map_dbl(draws, sd),
    lower = purrr::map_dbl(draws, ~quantile(.x, level_lower)),
    upper = purrr::map_dbl(draws, ~quantile(.x, level_upper))
  )
  mcse <- tibble::tibble(
    group = group_names(draws),
    time = time_names(draws),
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
    group = group_names(draws),
    time = time_names(draws),
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

marginal_name <- function(group, time) {
  sprintf("%s, %s", group , time)
}

group_names <- function(draws) {
  gsub(",.*$", "", colnames(draws))
}

time_names <- function(draws) {
  gsub("^.*, ", "", colnames(draws))
}
