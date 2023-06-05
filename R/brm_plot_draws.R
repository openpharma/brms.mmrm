#' @title Visualize posterior draws of marginals.
#' @export
#' @family visualization
#' @description Visualize posterior draws of marginals.
#' @return A `ggplot` object.
#' @param draws A data frame of draws from an element of
#'   the output list of [brm_marginal_summaries()].
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
#' draws <- brm_marginal_draws(
#'   model = model,
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   control = "treatment 1",
#'   baseline = "visit 1",
#'   outcome = "response"
#' )
#' brm_plot_draws(draws = draws$change)
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
