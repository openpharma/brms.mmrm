#' @title MCMC draws from the marginal posterior of an MMRM
#' @export
#' @family marginals
#' @description Get marginal posterior draws from a fitted MMRM.
#' @inheritSection brm_data Separation string
#' @return A named list of tibbles of MCMC draws of the marginal posterior
#'   distribution of each treatment group and time point
#'   (or group-by-subgroup-by-time, if applicable).
#'   In each tibble, there is 1 row per posterior sample and one column for
#'   each type of marginal distribution (i.e. each combination of treatment
#'   group and discrete time point. The specific `tibble`s in the returned
#'   list are described below:
#'   * `response`: on the scale of the response variable.
#'   * `difference_time`: change from baseline: the
#'     `response` at a particular time minus the `response` at baseline
#'     (`reference_time`).
#'     Only returned if the `role` argument of [brm_data()] was
#'     `"response"`. (If `role` is `"change"`, then `response` already
#'     represents change from baseline.)
#'   * `difference_group`: treatment effect: the
#'     the `difference_time` at each active group minus the `difference_time`
#'     at the control group (`reference_group`).
#'     If `role` is `"change"`, then treatment group
#'     is instead the difference between `response` at each active group minus
#'     the `response` at the control group.
#'   * `difference_subgroup`: subgroup differences: the `difference_group`
#'     at each subgroup level minus the `difference_group` at the subgroup
#'     reference level (`reference_subgroup`).
#' @param model A fitted model object from [brm_model()].
#' @param data Classed tibble with preprocessed data from [brm_data()].
#' @param use_subgroup Deprecated. No longer used. [brm_marginal_draws()]
#'   no longer marginalizes over the subgroup declared
#'   in [brm_data()]. To marginalize over the subgroup, declare
#'   that variable in `covariates` instead.
#' @param control Deprecated. Set the control group level in [brm_data()].
#' @param baseline Deprecated. Set the control group level in [brm_data()].
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
#' brm_marginal_draws(model = model, data = data)
#' }
brm_marginal_draws <- function(
  model,
  data,
  use_subgroup = NULL,
  control = NULL,
  baseline = NULL
) {
  if (!is.null(use_subgroup)) {
    brm_deprecate(
      "The use_subgroup argument is deprectaed. brm_marginal_draws()",
      "no longer marginalizes over the subgroup declared in brm_data().",
      "To marginalize over the subgroup, declare that variable",
      "in 'covariates' instead."
    )
  }
  if (!is.null(control)) {
    brm_deprecate(
      "The control argument was deprecated on 2023-09-07. ",
      "Set the reference_group argument of brm_data() instead."
    )
  }
  if (!is.null(baseline)) {
    brm_deprecate(
      "The baseline argument was deprecated on 2023-09-07. ",
      "Set the reference_time argument of brm_data() instead."
    )
  }
  brm_model_validate(model)
  brm_data_validate(data)
  role <- attr(data, "brm_role")
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  levels_group <- attr(data, "brm_levels_group")
  levels_subgroup <- attr(data, "brm_levels_subgroup")
  levels_time <- attr(data, "brm_levels_time")
  reference_group <- attr(data, "brm_reference_group")
  reference_subgroup <- attr(data, "brm_reference_subgroup")
  reference_time <- attr(data, "brm_reference_time")
  has_subgroup <- !is.null(subgroup)
  nuisance <- c(
    base,
    patient,
    covariates,
    if_any(has_subgroup, NULL, subgroup)
  )
  specs <- if_any(
    has_subgroup,
    as.formula(sprintf("~%s:%s:%s", group, subgroup, time)),
    as.formula(sprintf("~%s:%s", group, time))
  )
  emmeans <- emmeans::emmeans(
    object = model,
    specs = specs,
    wt.nuis = "proportional",
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
    if (has_subgroup) {
      draws_difference_time <- subtract_reference_time_subroup(
        draws = draws_response,
        levels_group = levels_group,
        levels_subgroup = levels_subgroup,
        levels_time = levels_time,
        reference_time = reference_time
      )
      draws_difference_group <- subtract_reference_group_subgroup(
        draws = draws_difference_time,
        levels_group = levels_group,
        levels_subgroup = levels_subgroup,
        levels_time = setdiff(levels_time, reference_time),
        reference_group = reference_group
      )
      draws_difference_subgroup <- subtract_reference_subgroup(
        draws = draws_difference_group,
        levels_group = setdiff(levels_group, reference_group),
        levels_subgroup = levels_subgroup,
        levels_time = setdiff(levels_time, reference_time),
        reference_subgroup = reference_subgroup
      )
    } else { # role is "response", no subgroup
      draws_difference_time <- subtract_reference_time(
        draws = draws_response,
        levels_group = levels_group,
        levels_time = levels_time,
        reference_time = reference_time
      )
      draws_difference_group <- subtract_reference_group(
        draws = draws_difference_time,
        levels_group = levels_group,
        levels_time = setdiff(levels_time, reference_time),
        reference_group = reference_group
      )
    }
  } else { # role is "change"
    if (has_subgroup) {
      draws_difference_group <- subtract_reference_group_subgroup(
        draws = draws_response,
        levels_group = levels_group,
        levels_subgroup = levels_subgroup,
        levels_time = levels_time,
        reference_group = reference_group
      )
      draws_difference_subgroup <- subtract_reference_subgroup(
        draws = draws_difference_group,
        levels_group = setdiff(levels_group, reference_group),
        levels_subgroup = levels_subgroup,
        levels_time = levels_time,
        reference_subgroup = reference_subgroup
      )
    } else { # role is "change", no subgroup
      draws_difference_group <- subtract_reference_group(
        draws = draws_response,
        levels_group = levels_group,
        levels_time = levels_time,
        reference_group = reference_group
      )
    }
  }
  draws_sigma <- get_draws_sigma(model = model, time = time)
  draws_effect <- if_any(
    has_subgroup,
    get_draws_effect_subgroup(
      draws_difference_group = draws_difference_group,
      draws_sigma = draws_sigma,
      levels_group = levels_group,
      levels_subgroup = levels_subgroup,
      levels_time = levels_time
    ),
    get_draws_effect(
      draws_difference_group = draws_difference_group,
      draws_sigma = draws_sigma,
      levels_group = levels_group,
      levels_time = levels_time
    )
  )
  out <- list()
  out$response <- draws_response
  if (identical(role, "response")) {
    out$difference_time <- draws_difference_time
  }
  out$difference_group <- draws_difference_group
  if (has_subgroup) {
    out$difference_subgroup <- draws_difference_subgroup
  }
  out$effect <- draws_effect
  out
}

get_draws_sigma <- function(model, time) {
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  colnames(draws) <- gsub("^b_sigma_", "", colnames(draws))
  colnames(draws) <- gsub(paste0("^", time), "", x = colnames(draws))
  exp(draws)
}

get_draws_effect <- function(
  draws_difference_group,
  draws_sigma,
  levels_group,
  levels_time
) {
  out <- draws_difference_group
  for (group in levels_group) {
    for (time in levels_time) {
      name <- name_marginal(group = group, time = time)
      if (name %in% colnames(draws_difference_group)) {
        out[[name]] <- draws_difference_group[[name]] / draws_sigma[[time]]
      }
    }
  }
  out
}

get_draws_effect_subgroup <- function(
  draws_difference_group,
  draws_sigma,
  levels_group,
  levels_subgroup,
  levels_time
) {
  out <- draws_difference_group
  for (group in levels_group) {
    for (subgroup in levels_subgroup) {
      for (time in levels_time) {
        name <- name_marginal_subgroup(
          group = group,
          subgroup = subgroup,
          time = time
        )
        if (name %in% colnames(draws_difference_group)) {
          out[[name]] <- draws_difference_group[[name]] / draws_sigma[[time]]
        }
      }
    }
  }
  out
}

subtract_reference_time <- function(
  draws,
  levels_group,
  levels_time,
  reference_time
) {
  out <- draws[, names_mcmc]
  for (group in levels_group) {
    for (time in setdiff(levels_time, reference_time)) {
      name1 <- name_marginal(group, reference_time)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

subtract_reference_time_subroup <- function(
  draws,
  levels_group,
  levels_subgroup,
  levels_time,
  reference_time
) {
  out <- draws[, names_mcmc]
  for (group in levels_group) {
    for (subgroup in levels_subgroup) {
      for (time in setdiff(levels_time, reference_time)) {
        name1 <- name_marginal_subgroup(group, subgroup, reference_time)
        name2 <- name_marginal_subgroup(group, subgroup, time)
        out[[name2]] <- draws[[name2]] - draws[[name1]]
      }
    }
  }
  out
}

subtract_reference_group <- function(
  draws,
  levels_group,
  levels_time,
  reference_group
) {
  out <- draws[, names_mcmc]
  for (group in setdiff(levels_group, reference_group)) {
    for (time in levels_time) {
      name1 <- name_marginal(reference_group, time)
      name2 <- name_marginal(group, time)
      out[[name2]] <- draws[[name2]] - draws[[name1]]
    }
  }
  out
}

subtract_reference_group_subgroup <- function(
  draws,
  levels_group,
  levels_subgroup,
  levels_time,
  reference_group
) {
  out <- draws[, names_mcmc]
  for (group in setdiff(levels_group, reference_group)) {
    for (subgroup in levels_subgroup) {
      for (time in levels_time) {
        name1 <- name_marginal_subgroup(reference_group, subgroup, time)
        name2 <- name_marginal_subgroup(group, subgroup, time)
        out[[name2]] <- draws[[name2]] - draws[[name1]]
      }
    }
  }
  out
}

subtract_reference_subgroup <- function(
  draws,
  levels_group,
  levels_subgroup,
  levels_time,
  reference_subgroup
) {
  out <- draws[, names_mcmc]
  for (group in levels_group) {
    for (subgroup in setdiff(levels_subgroup, reference_subgroup)) {
      for (time in levels_time) {
        name1 <- name_marginal_subgroup(group, reference_subgroup, time)
        name2 <- name_marginal_subgroup(group, subgroup, time)
        out[[name2]] <- draws[[name2]] - draws[[name1]]
      }
    }
  }
  out
}
