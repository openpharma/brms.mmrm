#' @title Summarize an MMRM.
#' @export
#' @family results
#' @description Summarize a basic MMRM model fit.
#' @details Currently assumes the response variable is `CHG`
#'   (change from baseline) and not `AVAL` (raw response).
#' @return A `tibble` with summary statistics of the marginal posterior.
#' @inheritParams brm_formula
#' @param model Fitted `brms` model object from [brm_model()].
#' @param control Element of the `group` column in the data which indicates
#'   the control group for the purposes of calculating treatment differences.
#' @param baseline Element of the `time` column in the data
#'   which indicates the baseline time for the purposes of calculating
#'   change from baseline. Ignored if `response_type = "change"`.
#' @param response_type Character of length 1, `"response"` if the
#'   response variable is the raw outcome variable (such as AVAL)
#'   or `"change"` if the response variable is change from baseline
#'   (e.g. CHG).
#' @param level Numeric of length 1 between 0 and 1, credible level
#'   for the credible intervals in the output.
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
#'   response_type = "response"
#' )
brm_summary <- function(
  model,
  base = "BASE",
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0),
  control = "Placebo",
  baseline = "Baseline",
  response_type = "change",
  level = 0.95
) {
  assert_chr(base, "base arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_chr(patient, "patient arg must be a nonempty character string")
  assert_chr(
    response_type,
    "response_type arg must be a nonempty character string"
  )
  assert(
    response_type %in% c("response", "change"),
    message = "response_type must be either \"response\" or \"change\""
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
  groups <- unique(gsub(",.*$", "", colnames(draws_response)))
  times <- unique(gsub("^.*, ", "", colnames(draws_response)))
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
  if (response_type == "response") {
    assert(
      baseline %in% times,
      message = sprintf(
        "baseline argument \"%s\" is not in one of the time points: %s",
        baseline,
        paste(times, collapse = ", ")
      )
    )
  }
  if (response_type == "response") {
    draws_change <- subtract_baseline(
      draws = draws_response,
      groups = groups,
      times = times,
      baseline = baseline
    )
    draws_diff <- subtract_control(
      draws = draws_change,
      groups = groups,
      times = setdiff(times, baseline),
      control = control
    )
  } else {
    draws_diff <- subtract_control(
      draws = draws_response,
      groups = groups,
      times = times,
      control = control
    )
  }
  table_response <- summarize_marginals(draws_response, level)
  table_change <- if_any(
    response_type == "response",
    summarize_marginals(draws_change, level),
    NULL
  )
  table_diff <- summarize_marginals(draws_diff, level)
  dplyr::bind_rows(
    response = table_response,
    change = table_change,
    difference = table_diff,
    .id = "marginal"
  )
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

marginal_name <- function(group, time) {
  sprintf("%s, %s", group , time)
}

summarize_marginals <- function(draws, level) {
  level_lower <- (1 - level) / 2
  level_upper <- 1 - level_lower
  draws[[".chain"]] <- NULL
  draws[[".iteration"]] <- NULL
  draws[[".draw"]] <- NULL
  value <- tibble::tibble(
    group = gsub(",.*$", "", colnames(draws)),
    time = gsub("^.*, ", "", colnames(draws)),
    mean = purrr::map_dbl(draws, mean),
    median = purrr::map_dbl(draws, median),
    sd = purrr::map_dbl(draws, sd),
    lower = purrr::map_dbl(draws, ~quantile(.x, level_lower)),
    upper = purrr::map_dbl(draws, ~quantile(.x, level_upper))
  )
  mcse <- tibble::tibble(
    group = gsub(",.*$", "", colnames(draws)),
    time = gsub("^.*, ", "", colnames(draws)),
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
