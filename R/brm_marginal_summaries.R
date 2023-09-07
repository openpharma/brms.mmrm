#' @title Summary statistics of the marginal posterior of an MMRM.
#' @export
#' @family marginals
#' @description Summary statistics of the marginal posterior of an MMRM.
#' @return A tibble with one row per summary statistic and the following
#'   columns:
#'    * `marginal`: type of marginal distribution. If `outcome` was `"response"`
#'      in [brm_marginal_draws()], then possible values include
#'      `"response"` for the response on the raw scale, `"change"` for
#'      change from baseline, and `"difference"` for treatment difference
#'      in terms of change from baseline. If `outcome` was `"change"`,
#'      then possible values include `"response"` for the response one the
#'      change from baseline scale and `"difference"` for treatment difference.
#'    * `statistic`: type of summary statistic. `"lower"` and `"upper"`
#'      are bounds of an equal-tailed quantile-based credible interval.
#'    * `group`: treatment group.
#'    * `time`: discrete time point.
#'    * `value`: numeric value of the estimate.
#'    * `mcse`: Monte Carlo standard error of the estimate.
#'   The `statistic` column has the following possible values:
#'    * `mean`: posterior mean.
#'    * `median`: posterior median.
#'    * `sd`: posterior standard deviation of the mean.
#'    * `lower`: lower bound of an equal-tailed credible interval of the mean,
#'      with credible level determined by the `level` argument.
#'    * `upper`: upper bound of an equal-tailed credible interval
#'      with credible level determined by the `level` argument.
#' @param draws Posterior draws of the marginal posterior
#'   obtained from [brm_marginal_draws()].
#' @param level Numeric of length 1 between 0 and 1, credible level
#'   for the credible intervals.
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
#'   level_control = "group_1",
#'   level_baseline = "time_1"
#' )
#' formula <- brm_formula(
#'   data = data,
#'   effect_baseline = FALSE,
#'   interaction_baseline = FALSE
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
#' suppressWarnings(brm_marginal_summaries(draws))
#' }
brm_marginal_summaries <- function(
  draws,
  level = 0.95
) {
  assert(
    is.list(draws),
    message = "marginals arg must be a named list from brm_marginal_draws()"
  )
  assert_num(level, "level arg must be a length-1 numeric between 0 and 1")
  assert(level, . >= 0, . <= 1, message = "level arg must be between 0 and 1")
  table_response <- summarize_marginals(draws$response, level)
  table_change <- if_any(
    "change" %in% names(draws),
    summarize_marginals(draws$change, level),
    NULL
  )
  table_difference <- summarize_marginals(draws$difference, level)
  table_effect <- summarize_marginals(draws$effect, level)
  out <- dplyr::bind_rows(
    response = table_response,
    change = table_change,
    difference = table_difference,
    effect = table_effect,
    .id = "marginal"
  )
  columns <- c("marginal", "statistic", "group", "time", "value", "mcse")
  out <- out[, columns]
  args <- lapply(setdiff(columns, c("value", "mcse")), as.symbol)
  args$.data <- out
  do.call(what = dplyr::arrange, args = args)
}

summarize_marginals <- function(draws, level) {
  level_lower <- (1 - level) / 2
  level_upper <- 1 - level_lower
  draws[names_mcmc] <- NULL
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
    cols = -tidyselect::any_of(c("group", "time")),
    names_to = "statistic",
    values_to = "value"
  )
  mcse <- tidyr::pivot_longer(
    data = mcse,
    cols = -tidyselect::any_of(c("group", "time")),
    names_to = "statistic",
    values_to = "mcse"
  )
  out <- dplyr::left_join(
    x = value,
    y = mcse,
    by = c("group", "time", "statistic")
  )
  unname_df(out)
}
