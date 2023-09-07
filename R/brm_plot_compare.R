#' @title Visually compare the marginals of models and datasets.
#' @export
#' @family visualization
#' @description Visually compare the marginals of models and datasets.
#' @return A `ggplot` object.
#' @param ... Named data frames of marginals posterior summaries
#'   from [brm_marginal_summaries()] and/or [brm_marginal_data()].
#' @param marginal Character of length 1, which kind of marginal
#'   to visualize: either `"response"`, `"change"`, or `"difference"`.
#'   Only applies to MCMC output, the data is always on the scale of the
#'   response variable.
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
#' draws <- brm_marginal_draws(model = model, data = data)
#' suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
#' summaries_data <- brm_marginal_data(data)
#' brm_plot_compare(
#'   model1 = summaries_draws,
#'   model2 = summaries_draws,
#'   data = summaries_data
#' )
#' brm_plot_compare(
#'   model1 = summaries_draws,
#'   model2 = summaries_draws,
#'   marginal = "difference"
#' )
#' }
brm_plot_compare <- function(..., marginal = "response") {
  data <- list(...)
  assert_chr(marginal, "marginal arg must be a nonempty character string.")
  assert(
    marginal %in% c("response", "change", "difference"),
    message =
      "marginal arg must be one of \"response\", \"change\", or \"difference\""
  )
  assert_chr_vec(names(data), message = "arguments must be named.")
  for (name in names(data)) {
    data[[name]] <- data_compare_clean(data[[name]], marginal = marginal)
  }
  data <- dplyr::bind_rows(data, .id = "source")
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
    ggplot2::ylab(marginal) +
    ggplot2::theme_gray(16)
}

data_compare_clean <- function(data, marginal) {
  if ("marginal" %in% colnames(data)) {
    data <- data[data$marginal == marginal, ]
  }
  data <- data[data$statistic %in% c("mean", "lower", "upper"), ]
  data[, c("statistic", "group", "time", "value")]
}
