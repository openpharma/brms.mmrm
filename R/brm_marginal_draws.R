#' @title MCMC draws from the marginal posterior of an MMRM
#' @export
#' @family marginals
#' @description Get marginal posterior draws from a fitted MMRM.
#' @section Baseline:
#'   The returned values from [brm_marginal_draws()]
#'   depend on whether a baseline time point
#'   was declared through the `reference_time` argument of [brm_data()].
#'   If `reference_time` was not `NULL`, then [brm_marginal_draws()] will
#'   calculate change from baseline, and it will calculate treatment
#'   differences as differences between change-from-baseline values.
#'   If `reference_time` was not `NULL`, then [brm_marginal_draws()] will
#'   not calculate change from baseline, and it will calculate treatment
#'   differences as differences between response values.
#' @inheritSection brm_data Separation string
#' @return A named list of tibbles of MCMC draws of the marginal posterior
#'   distribution of each treatment group and time point. These marginals
#'   are also subgroup-specific if [brm_formula()] included fixed effects
#'   that use the `subgroup` variable originally declared in [brm_data()].
#'   In each tibble, there is 1 row per posterior sample and one column for
#'   each type of marginal distribution (i.e. each combination of treatment
#'   group and discrete time point. The specific `tibble`s in the returned
#'   list are described below:
#'   * `response`: on the scale of the response variable.
#'   * `difference_time`: change from baseline: the
#'     `response` at a particular time minus the `response` at baseline
#'     (`reference_time`).
#'     Only returned if the `reference_time` argument of [brm_data()] was
#'     not `NULL` (i.e. if a baseline value for the time variable
#'     was identified).
#'   * `difference_group`: treatment effect:
#'     These samples depend on the values of `reference_group` and
#'     `reference_time` which were originally declared in [brm_data()].
#'     `reference_group` is the control group, and `reference_time`
#'     is baseline. If baseline was originally given (via `reference_time`
#'     in [brm_data()]),
#'     then `difference_time` is the change-from-baseline value of
#'     each active group minus that of the control group.
#'     Otherwise, if baseline is omitted (i.e. `reference_time = NULL`
#'     (default) in [brm_data()]), then `difference_time` is the
#'     raw response at each active group minus that of the control group.
#'   * `difference_subgroup`: subgroup differences: the `difference_group`
#'     at each subgroup level minus the `difference_group` at the subgroup
#'     reference level (`reference_subgroup`). Only reported if a subgroup
#'     analysis was specified through the appropriate arguments to
#'     [brm_data()] and [brm_formula()].
#'   * `effect`: effect size, defined as the treatment difference
#'     divided by the residual standard deviation. Omitted if
#'     the `effect_size` argument is `FALSE` or if the
#'     [brm_formula_sigma()] includes baseline or covariates.
#'   * `sigma`: posterior draws of linear-scale marginal standard deviations
#'     of residuals. Omitted if
#'     the `effect_size` argument is `FALSE` or if the
#'     [brm_formula_sigma()] includes baseline or covariates.
#' @inheritParams brm_model
#' @param model A fitted model object from [brm_model()].
#' @param transform Matrix with one row per marginal mean and one column
#'   per model parameter. [brm_marginal_draws()] uses this matrix
#'   to map posterior draws of model parameters to posterior draws of
#'   marginal means using matrix multiplication. Please use
#'   [brm_transform_marginal()] to compute this matrix and then modify
#'   only if necessary. See the methods vignettes for details on this
#'   matrix, as well as how `brms.mmrm` computes marginal means more
#'   generally.
#' @param effect_size Logical, `TRUE` to derive posterior samples
#'   of effect size (treatment effect divided by residual standard
#'   deviation). `FALSE` to omit. `brms.mmrm` does not support
#'   effect size when baseline or covariates are included
#'   in the [brm_formula_sigma()] formula. If `effect_size` is `TRUE`
#'   in this case, then [brm_marginal_draws()] will automatically
#'   omit effect size and throw an informative warning.
#' @param use_subgroup Deprecated. No longer used. [brm_marginal_draws()]
#'   no longer marginalizes over the subgroup declared
#'   in [brm_data()]. To marginalize over the subgroup, declare
#'   that variable in `covariates` instead.
#' @param average_within_subgroup `TRUE`, `FALSE`, or `NULL` to control
#'   whether nuisance parameters are averaged within subgroup levels
#'   in [brm_transform_marginal()]. Ignored if the `transform` argument
#'   is manually supplied by the user. See the help page of
#'   [brm_transform_marginal()] for details on the
#'   `average_within_subgroup` argument.
#' @param control Deprecated. Set the control group level in [brm_data()].
#' @param baseline Deprecated. Set the control group level in [brm_data()].
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_data(
#'   data = brm_simulate_simple()$data,
#'   outcome = "response",
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
#' brm_marginal_draws(data = data, formula = formula, model = model)
#' }
brm_marginal_draws <- function(
  model,
  data = model$brms.mmrm_data,
  formula = model$brms.mmrm_formula,
  transform = brms.mmrm::brm_transform_marginal(
    data = data,
    formula = formula,
    average_within_subgroup = average_within_subgroup
  ),
  effect_size = attr(formula, "brm_allow_effect_size"),
  average_within_subgroup = NULL,
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
  assert(
    effect_size,
    is.logical(.),
    !anyNA(.),
    length(.) == 1L,
    message = "effect_size must be TRUE or FALSE."
  )
  brm_data_validate(data)
  brm_formula_validate(formula)
  brm_model_validate(model)
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- if_any(
    is.null(subgroup),
    character(0L),
    brm_levels(data[[subgroup]])
  )
  levels_time <- brm_levels(data[[time]])
  reference_group <- attr(data, "brm_reference_group")
  reference_subgroup <- attr(data, "brm_reference_subgroup")
  reference_time <- attr(data, "brm_reference_time")
  has_subgroup <- brm_has_subgroup(data = data, formula = formula)
  has_baseline <- !is.null(reference_time)
  if (effect_size && !attr(formula, "brm_allow_effect_size")) {
    effect_size <- FALSE
    brm_warn(
      "effect_size is TRUE in brm_marginal_draws(), ",
      "but the formula from brm_formula_sigma() includes ",
      "baseline or covariates. Effect size is only supported ",
      "when the brm_formula_sigma() formula does not have ",
      "baseline or covariates. Omitting effect size from the output."
    )
  }
  draws_model <- posterior::as_draws_df(model)
  index_mcmc <- tibble::as_tibble(draws_model)[, names_mcmc]
  draws_beta <- dplyr::select(
    .data = tibble::as_tibble(draws_model),
    tidyselect::starts_with("b_"),
    -tidyselect::starts_with("b_sigma")
  )
  assert(
    sort(colnames(transform)) == sort(colnames(draws_beta)),
    message = paste(
      "In brm_marginal_draws(), the column names of the 'transform'",
      "argument disagree with the names of the brms model parameters.",
      "Please use brm_transform_marginal() to compute the transform",
      "and do not modify it except for specialized circumstances.",
      "If you did use brm_transform_marginal() and supply it directly",
      "to brms_marginal_draws(), please submit a bug report",
      "at https://github.com/openpharma/brms.mmrm along with a",
      "small runnable example that reproduces the issue."
    )
  )
  draws_beta <- as.matrix(draws_beta[, colnames(transform)])
  assert(colnames(transform) == colnames(draws_beta))
  draws_response <- tibble::as_tibble(as.matrix(draws_beta) %*% t(transform))
  draws_response <- dplyr::bind_cols(draws_response, index_mcmc)
  draws_response <- posterior::as_draws_df(draws_response)
  if (has_baseline) { # baseline exists, subgroup exists
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
    } else { # baseline exists, no subgroup
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
  } else { # baseline does not exist, subgroup exists
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
    } else { # baseline does not exist, no subgroup
      draws_difference_group <- subtract_reference_group(
        draws = draws_response,
        levels_group = levels_group,
        levels_time = levels_time,
        reference_group = reference_group
      )
    }
  }
  if (effect_size) {
    draws_sigma <- get_draws_sigma(
      data = data,
      formula = formula,
      model = model,
      group = group,
      subgroup = subgroup,
      time = time,
      has_subgroup = has_subgroup
    )
    draws_effect <- if_any(
      has_subgroup,
      get_draws_effect_subgroup(
        draws_difference_group = draws_difference_group,
        draws_sigma = draws_sigma,
        levels_group = levels_group,
        levels_subgroup = levels_subgroup,
        levels_time = levels_time,
        variance = attr(formula, "brm_variance")
      ),
      get_draws_effect(
        draws_difference_group = draws_difference_group,
        draws_sigma = draws_sigma,
        levels_group = levels_group,
        levels_time = levels_time,
        variance = attr(formula, "brm_variance")
      )
    )
  }
  out <- list()
  out$response <- draws_response
  if (has_baseline) {
    out$difference_time <- draws_difference_time
  }
  out$difference_group <- draws_difference_group
  if (has_subgroup) {
    out$difference_subgroup <- draws_difference_subgroup
  }
  if (effect_size) {
    out$effect <- draws_effect
    out$sigma <- draws_sigma
  }
  out
}

get_draws_sigma <- function(
  data,
  formula,
  model,
  group,
  subgroup,
  time,
  has_subgroup
) {
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  x <- brms::make_standata(formula = formula, data = data)$X_sigma
  colnames(x) <- paste0("b_", colnames(x))
  x <- as.data.frame(x)
  x$name <- if_any(
    has_subgroup,
    name_marginal_subgroup(
      group = data[[group]],
      subgroup = data[[subgroup]],
      time = data[[time]]
    ),
    name_marginal(group = data[[group]], time = data[[time]])
  )
  x <- dplyr::group_by(x, name)
  transform <- dplyr::summarize(
    .data = x,
    dplyr::across(tidyselect::starts_with("b_"), mean),
    .groups = "drop"
  )
  name <- transform$name
  transform$name <- NULL
  transform <- as.matrix(transform)
  out <- tibble::as_tibble(as.data.frame(as.matrix(draws) %*% t(transform)))
  colnames(out) <- name
  exp(out)
}

get_draws_effect <- function(
  draws_difference_group,
  draws_sigma,
  levels_group,
  levels_time,
  variance
) {
  out <- draws_difference_group
  for (group in levels_group) {
    for (time in levels_time) {
      name <- name_marginal(group = group, time = time)
      if (name %in% colnames(draws_difference_group)) {
        out[[name]] <- draws_difference_group[[name]] / draws_sigma[[name]]
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
  levels_time,
  variance
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
          out[[name]] <- draws_difference_group[[name]] / draws_sigma[[name]]
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
