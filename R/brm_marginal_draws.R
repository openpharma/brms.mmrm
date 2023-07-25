#' @title MCMC draws from the marginal posterior of an MMRM
#' @export
#' @family marginals
#' @description Get marginal posterior draws from a fitted MMRM.
#' @inheritSection brm_data Separation string
#' @return A named list of tibbles of MCMC draws of the marginal posterior
#'   distribution of each treatment group and time point:
#'   * `response`: on the scale of the response variable.
#'   * `change`: change from baseline, where the `baseline` argument determines
#'     the time point at baseline. Only returned if the `role` argument is
#'     `"response"`. (If `role` is `"change"`, then `response` already
#'     represents change from baseline.)
#'   * `difference`: treatment effect of change from baseline, where the
#'     `control` argument identifies the placebo or active control group.
#'   In each tibble, there is 1 row per posterior sample and one column for
#'   each type of marginal distribution (i.e. each combination of treatment
#'   group and discrete time point.
#'   Treatment and time are comma-delimited in the column names.
#' @param model Fitted `brms` model object from [brm_model()].
#' @param data Classed tibble with preprocessed data from [brm_data()].
#' @param control Element of the `group` column in the data which indicates
#'   the control group for the purposes of calculating treatment differences.
#'   Elements in `data[[group]]` are already pre-processed by [brm_data()],
#'   so `control` is automatically sanitized accordingly using
#'   `make.names(control, unique = FALSE, allow_ = TRUE)`.
#' @param baseline Element of the `time` column in the data
#'   which indicates the baseline time for the purposes of calculating
#'   change from baseline.
#'   Elements in `data[[group]]` are already pre-processed by [brm_data()],
#'   so `control` is automatically sanitized accordingly using
#'   `make.names(control, unique = FALSE, allow_ = TRUE)`.
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_data(
#'   data = brm_simulate()$data,
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient"
#' )
#' formula <- brm_formula(
#'   data = data,
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
#'   data = data,
#'   control = "group 1",
#'   baseline = "time 1"
#' )
#' }
brm_marginal_draws <- function(
  model,
  data,
  control = "Placebo",
  baseline = "Baseline"
) {
  brm_data_validate(data)
  role <- attr(data, "brm_role")
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  levels_group <- attr(data, "brm_levels_group")
  levels_time <- attr(data, "brm_levels_time")
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
  control <- brm_names(control)
  baseline <- brm_names(baseline)
  assert(
    control %in% as.character(data[[group]]),
    message = "control arg must be a treatment group level in the data"
  )
  if (identical(role, "response")) {
    assert(
      baseline %in% as.character(data[[time]]),
      message = "baseline arg must be a discrete time level in the data"
    )
  }
  nuisance <- c(base, patient, covariates)
  emmeans <- emmeans::emmeans(
    object = model,
    specs = as.formula(sprintf("~%s:%s", group, time)),
    weights = "proportional",
    nuisance = nuisance
  )
  old_sep <- emmeans::get_emm_option("sep")
  on.exit(emmeans::emm_options(sep = old_sep))
  emmeans::emm_options(sep = brm_sep())
  mcmc <- coda::as.mcmc(emmeans, fixed = TRUE, names = FALSE)
  draws_response <- posterior::as_draws_df(mcmc)
  assert(
    control %in% levels_group,
    message = sprintf(
      "control argument \"%s\" is not in one of the treatment groups: %s",
      control,
      paste(levels_group, collapse = ", ")
    )
  )
  if (identical(role, "response")) {
    assert(
      baseline %in% levels_time,
      message = sprintf(
        "baseline argument \"%s\" is not in one of the time points: %s",
        baseline,
        paste(levels_time, collapse = ", ")
      )
    )
    draws_change <- subtract_baseline(
      draws = draws_response,
      levels_group = levels_group,
      levels_time = levels_time,
      baseline = baseline
    )
    draws_difference <- subtract_control(
      draws = draws_change,
      levels_group = levels_group,
      levels_time = setdiff(levels_time, baseline),
      control = control
    )
  } else {
    draws_difference <- subtract_control(
      draws = draws_response,
      levels_group = levels_group,
      levels_time = levels_time,
      control = control
    )
  }
  out <- list()
  out$response <- draws_response
  if (identical(role, "response")) {
    out$change <- draws_change
  }
  out$difference <- draws_difference
  out
}

subtract_baseline <- function(draws, levels_group, levels_time, baseline) {
  out <- draws[, names_mcmc]
  for (group in levels_group) {
    for (time in setdiff(levels_time, baseline)) {
      name1 <- name_marginal(group, baseline)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

subtract_control <- function(draws, levels_group, levels_time, control) {
  out <- draws[, names_mcmc]
  for (group in setdiff(levels_group, control)) {
    for (time in levels_time) {
      name1 <- name_marginal(control, time)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}
