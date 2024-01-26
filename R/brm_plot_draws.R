#' @title Visualize posterior draws of marginals.
#' @export
#' @family visualization
#' @description Visualize posterior draws of marginals.
#' @return A `ggplot` object.
#' @param draws A data frame of draws from an element of
#'   the output list of [brm_marginal_summaries()].
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
#' brm_plot_draws(draws = draws$difference_time)
#' }
brm_plot_draws <- function(draws) {
  assert(is.data.frame(draws), message = "draws argument must be a data frame.")
  draws <- tibble::as_tibble(draws)
  for (name in names_mcmc) {
    draws[[name]] <- NULL
  }
  names_group <- as.list(names_component(colnames(draws), "group"))
  names_time <- as.list(names_component(colnames(draws), "time"))
  names(names_group) <- colnames(draws)
  names(names_time) <- colnames(draws)
  use_subgroup <- names_have_subgroup(colnames(draws))
  if (use_subgroup) {
    names_subgroup <- as.list(names_component(colnames(draws), "subgroup"))
    names(names_subgroup) <- colnames(draws)
  }
  draws <- pivot_longer(
    data = draws,
    cols = tidyselect::everything(),
    names_to = "name", # cannot use names_sep (regexp) with brm_sep() (fixed)
    values_to = "value"
  )
  draws$group <- vapply(
    draws$name,
    function(x) names_group[[x]],
    FUN.VALUE = character(1L)
  )
  if (use_subgroup) {
    draws$subgroup <- vapply(
      draws$name,
      function(x) names_subgroup[[x]],
      FUN.VALUE = character(1L)
    )
  }
  draws$time <- vapply(
    draws$name,
    function(x) names_time[[x]],
    FUN.VALUE = character(1L)
  )
  draws$name <- NULL
  ggplot2::ggplot(draws) +
    ggridges::geom_density_ridges2(
      ggplot2::aes(x = value, y = time),
      scale = 0.9,
      stat = "binline",
      bins = 20
    ) +
    ggplot2::theme_gray(16) +
    ggplot2::coord_flip() +
    if_any(
      use_subgroup,
      ggplot2::facet_grid(subgroup ~ group),
      ggplot2::facet_wrap(~ group)
    )
}
