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
#' draws <- brm_marginal_draws(
#'   model = model,
#'   data = data,
#'   control = "group 1",
#'   baseline = "time 1"
#' )
#' brm_plot_draws(draws = draws$change)
#' }
brm_plot_draws <- function(draws) {
  assert(is.data.frame(draws), message = "draws argument must be a data frame.")
  draws <- tibble::as_tibble(draws)
  for (name in names_mcmc) {
    draws[[name]] <- NULL
  }
  draws <- pivot_longer(
    data = draws,
    cols = tidyselect::everything(),
    names_to = "name",
    values_to = "value"
  )
  draws$group <- gsub_group(draws$name)
  draws$time <- gsub_time(draws$name)
  draws$name <- NULL
  ggplot2::ggplot(draws) +
    ggridges::geom_density_ridges2(
      ggplot2::aes(x = value, y = time),
      scale = 0.9,
      stat = "binline",
      bins = 20
    ) +
    ggplot2::facet_wrap(~ group) +
    ggplot2::theme_gray(16) +
    ggplot2::coord_flip()
}
