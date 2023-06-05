#' @title Visualize the model against the data.
#' @export
#' @family visualization
#' @description Visualize the model against the data.
#' @return A `ggplot` object.
#' @param summaries_draws Posterior summaries of marginal MCMC draws
#'   from [brm_marginal_summaries()].
#' @param summaries_data Posterior summaries of the data
#'   from [brm_marginal_data()].
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
#' suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
#' summaries_data <- brm_marginal_data(
#'   data,
#'   response = "response",
#'   group = "group",
#'   time = "time"
#' )
#' brm_plot_compare(summaries_draws, summaries_data)
brm_plot_compare <- function(summaries_draws, summaries_data) {
  summaries_data$source <- "data"
  summaries_draws <- summaries_draws[summaries_draws$marginal == "response", ]
  summaries_draws$source <- summaries_draws$marginal
  summaries_draws$marginal <- NULL
  data <- dplyr::bind_rows(summaries_draws, summaries_data)
  columns <- c("statistic", "group", "time", "value", "source")
  data <- data[data$statistic %in% c("mean", "lower", "upper"), columns]
  data <- tidyr::pivot_wider(
    data = data,
    id_cols = c("source", "group", "time"),
    names_from = "statistic",
    values_from = "value"
  )
  ggplot2::ggplot(data) +
    ggplot2::geom_point(
      ggplot2::aes(x = time, y = mean, color = source),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = time, ymin = lower, ymax = upper, color = source),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::facet_wrap(~ group) +
    ggplot2::xlab("time") +
    ggplot2::ylab("response") +
    ggplot2::theme_gray(16)
}
