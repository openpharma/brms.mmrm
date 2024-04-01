#' @title Average marginal MCMC draws across time points.
#' @export
#' @family marginals
#' @description Simple un-weighted arithmetic mean of
#'   marginal MCMC draws across time points.
#' @inheritSection brm_data Separation string
#' @return A named list of tibbles of MCMC draws of the marginal posterior
#'   distribution of each treatment group and time point
#'   (or group-by-subgroup-by-time, if applicable).
#'   In each tibble, there is 1 row per posterior sample sand one column for
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
#' @inheritParams brm_marginal_draws
#' @param draws List of posterior draws from [brm_marginal_draws()].
#' @param times Character vector of discrete time point levels
#'   over which to average the MCMC samples within treatment group levels.
#'   Set to `NULL` to average across all time points. Levels are automatically
#'   sanitized with `make.names(unique = FALSE, allow_ = TRUE)` to ensure
#'   agreement with `brms` variable names in downstream computations.
#' @param label Character of length 1, time point label for the averages.
#'   Automatically sanitized with `make.names(unique = FALSE, allow_ = TRUE)`.
#'   Must not conflict with any existing time point labels in the data
#'   after the label and time points are sanitized.
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
#' draws <- brm_marginal_draws(data = data, formula = formula, model = model)
#' brm_marginal_draws_average(draws = draws, data = data)
#' brm_marginal_draws_average(
#'   draws = draws,
#'   data = data,
#'   times = c("time_1", "time_2"),
#'   label = "mean"
#' )
#' }
brm_marginal_draws_average <- function(
  data,
  draws,
  times = NULL,
  label = "average"
) {
  assert(
    is.list(draws),
    message = "marginals arg must be a named list from brm_marginal_draws()"
  )
  brm_data_validate(data)
  levels_group <-  attr(data, "brm_levels_group")
  levels_subgroup <-  attr(data, "brm_levels_subgroup")
  levels_time <- brm_levels(unique(times %|||% attr(data, "brm_levels_time")))
  assert(
    levels_time,
    is.character(.),
    !anyDuplicated(.),
    !anyNA(.),
    nzchar(.),
    length(.) > 0L,
    all(. %in% attr(data, "brm_levels_time")),
    message = "times argument must be valid discrete time points from the data"
  )
  label <- brm_levels(label)
  assert(
    label,
    is.character(.),
    !anyNA(.),
    nzchar(.),
    length(.) == 1L,
    !any(. %in% attr(data, "brm_levels_time")),
    message = paste(
      "label must be a string and must not conflict",
      "with existing time point labels."
    )
  )
  levels_time <- brm_levels(levels_time)
  for (field in names(draws)) {
    draws[[field]] <- if_any(
      names_have_subgroup(names(draws[[field]])),
      brm_marginal_draws_average_df_subgroup(
        draws = draws[[field]],
        levels_group = levels_group,
        levels_subgroup = levels_subgroup,
        levels_time = levels_time,
        label = label
      ),
      brm_marginal_draws_average_df(
        draws = draws[[field]],
        levels_group = levels_group,
        levels_time = levels_time,
        label = label
      )
    )
  }
  draws
}

brm_marginal_draws_average_df <- function(
  draws,
  levels_group,
  levels_time,
  label
) {
  original_columns <- setdiff(colnames(draws), names_mcmc)
  for (group in levels_group) {
    names <- name_marginal(group, levels_time)
    names <- intersect(names, colnames(draws))
    if (length(names) > 0L) {
      subset <- tibble::as_tibble(draws)[, names, drop = FALSE]
      name <- name_marginal(group, label)
      draws[[name]] <- apply(X = subset, MARGIN = 1L, FUN = mean)
    }
  }
  draws[, setdiff(colnames(draws), original_columns), .drop = FALSE]
}

brm_marginal_draws_average_df_subgroup <- function(
  draws,
  levels_group,
  levels_subgroup,
  levels_time,
  label
) {
  original_columns <- setdiff(colnames(draws), names_mcmc)
  for (group in levels_group) {
    for (subgroup in levels_subgroup) {
      names <- name_marginal_subgroup(group, subgroup, levels_time)
      names <- intersect(names, colnames(draws))
      if (length(names) > 0L) {
        subset <- tibble::as_tibble(draws)[, names, drop = FALSE]
        name <- name_marginal_subgroup(group, subgroup, label)
        draws[[name]] <- apply(X = subset, MARGIN = 1L, FUN = mean)
      }
    }
  }
  draws[, setdiff(colnames(draws), original_columns), .drop = FALSE]
}
