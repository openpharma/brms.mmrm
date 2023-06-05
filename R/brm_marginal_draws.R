#' @title MCMC draws from the marginal posterior of an MMRM
#' @export
#' @family marginals
#' @description Get marginal posterior draws from a fitted MMRM.
#' @return A named list of tibbles of MCMC draws of the marginal posterior
#'   distribution of each treatment group and time point:
#'   * `response`: on the scale of the response variable.
#'   * `change`: change from baseline, where the `baseline` argument determines
#'     the time point at baseline. Only returned if the `outcome` argument is
#'     `"response"`. (If `outcome` is `"change"`, then `response` already
#'     represents change from baseline.)
#'   * `difference`: treatment effect of change from baseline, where the
#'     `control` argument identifies the placebo or active control group.
#'   In each tibble, there is 1 row per posterior sample and one column for
#'   each type of marginal distribution (i.e. each combination of treatment
#'   group and discrete time point.
#'   Treatment and time are comma-delimited in the column names.
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
#' brm_marginal_draws(
#'   model = model,
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   control = "treatment 1",
#'   baseline = "visit 1",
#'   outcome = "response"
#' )
brm_marginal_draws <- function(
  model,
  base = "BASE",
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0),
  outcome = "change",
  control = "Placebo",
  baseline = "Baseline"
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
  mcmc <- coda::as.mcmc(emmeans, fixed = TRUE, names = FALSE)
  draws_response <- posterior::as_draws_df(mcmc)
  groups <- unique(names_group(draws_response))
  times <- unique(names_time(draws_response))
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
  if (identical(outcome, "response")) {
    assert(
      baseline %in% times,
      message = sprintf(
        "baseline argument \"%s\" is not in one of the time points: %s",
        baseline,
        paste(times, collapse = ", ")
      )
    )
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
  out <- list()
  out$response <- draws_response
  if (identical(outcome, "response")) {
    out$change <- draws_change
  }
  out$difference <- draws_difference
  out
}

subtract_baseline <- function(draws, groups, times, baseline) {
  out <- draws[, names_mcmc]
  for (group in groups) {
    for (time in setdiff(times, baseline)) {
      name1 <- name_marginal(group, baseline)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

subtract_control <- function(draws, groups, times, control) {
  out <- draws[, names_mcmc]
  for (group in setdiff(groups, control)) {
    for (time in times) {
      name1 <- name_marginal(control, time)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

name_marginal <- function(group, time) {
  sprintf("%s, %s", group, time)
}

names_group <- function(draws) {
  gsub_group(setdiff(colnames(draws), names_mcmc))
}

names_time <- function(draws) {
  gsub_time(setdiff(colnames(draws), names_mcmc))
}

gsub_group <- function(names) {
  gsub(",.*$", "", names)
}

gsub_time <- function(names) {
  gsub("^.*, ", "", names)
}

names_mcmc <- c(".chain", ".draw", ".iteration")
