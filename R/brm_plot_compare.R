#' @title Visually compare the marginals of multiple models and/or datasets.
#' @export
#' @family visualization
#' @description Visually compare the marginals of multiple models
#'    and/or datasets.
#' @details By default, [brm_plot_compare()] compares multiple models
#'   and/or datasets side-by-side. The `compare` argument selects the primary
#'   comparison of interest, and arguments `axis` and `facet` control
#'   the arrangement of various other components of the plot.
#'   The subgroup variable is automatically included if and only if
#'   all the supplied marginal summaries have a subgroup column.
#' @return A `ggplot` object.
#' @param ... Named `tibble`s of marginals posterior summaries
#'   from [brm_marginal_summaries()] and/or [brm_marginal_data()].
#' @param marginal Character of length 1, which kind of marginal
#'   to visualize. Must be a value in the `marginal` column of the supplied
#'   `tibble`s in the `...` argument.
#'   Only applies to MCMC output, the data is always on the scale of the
#'   response variable.
#' @param compare Character of length 1 identifying the variable to display
#'   using back-to-back interval plots of different colors. This is
#'   the primary comparison of interest. Must be one of
#'   `"source"` (the source of the marginal summaries,
#'   e.g. a model or dataset), `"time"` or `"group"`
#'   (in the non-subgroup case). Can also be `"subgroup"`
#'   if all the marginal summaries are subgroup-specific.
#'   The value must not be in `axis` or `facet`.
#' @param axis Character of length 1 identifying the quantity to put
#'   on the horizontal axis. Must be be one of `"source"`
#'   (the source of the marginal summaries,
#'   e.g. a model or dataset), `"time"`,
#'   or `"group"` (in the non-subgroup case). If the marginals
#'   are subgroup-specific, then `axis` can also be `"subgroup"`.
#'   The value must not be in `compare` or `facet`.
#' @param facet Character vector of length 1 or 2 with quantities to
#'   generate facets. Each element must be `"source"`
#'   (the source of the marginal summaries,
#'   e.g. a model or dataset), `"time"`, `"group"`,
#'   or `"subgroup"`, and `c(axis, facet)` must all have unique elements.
#'   `"subgroup"` is automatically removed if not all the marginal summaries
#'   have a subgroup column. If `facet` has length 1, then faceting is wrapped.
#'   If `facet` has length 2, then faceting is in a grid,
#'   and the first element is horizontal facet.
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
brm_plot_compare <- function(
  ...,
  marginal = "response",
  compare = "source",
  axis = "time",
  facet = c("group", "subgroup")
) {
  data_list <- list(...)
  assert_chr_vec(names(data_list), message = "arguments must be named.")
  assert_chr(compare, "'compare' must be a single nonempty character string")
  assert_chr(axis, "'axis' must be a single nonempty character string")
  assert_chr_vec(
    facet,
    "'facet' must be a nonempty character vector with 1 or 2 elements"
  )
  assert(
    compare %in% c("source", "time", "group", "subgroup"),
    message = "'axis' must be \"source\", \"time\", \"group\", or \"subgroup\""
  )
  assert(
    axis %in% c("source", "time", "group", "subgroup"),
    message = "'axis' must be \"source\", \"time\", \"group\", or \"subgroup\""
  )
  assert(
    facet %in% c("source", "time", "group", "subgroup"),
    message = paste(
      "each element of 'facet' must be",
      "\"source\", \"time\", \"group\", or \"subgroup\""
    )
  )
  assert_chr(marginal, "marginal arg must be a nonempty character string.")
  marginal <- if_any(marginal == "change", "difference_time", marginal)
  marginal <- if_any(marginal == "difference", "difference_group", marginal)
  for (name in names(data_list)) {
    assert(
      tibble::is_tibble(data_list[[name]]),
      message = sprintf("'%s' is not a tibble", name)
    )
    data_list[[name]] <- data_compare_clean(
      data_list[[name]],
      marginal = marginal
    )
  }
  data <- dplyr::bind_rows(data_list, .id = "source")
  use_subgroup <- "subgroup" %in% colnames(data) &&
    !anyNA(data$subgroup)
  if (!use_subgroup) {
    facet <- setdiff(facet, "subgroup")
    for (name in names(data_list)) {
      assert(
        !("subgroup" %in% colnames(data_list[[name]])) ||
          length(unique(data_list[[name]]$subgroup)) == 1L,
        message = paste(
          "brm_plot_compare() is omitting the subgroup variable because",
          "not all marginal summaries have it,",
          "but marginal summaries",
          shQuote(name),
          "have more than one subgroup level. Please either filter",
          "on a single subgroup level or make sure all supplied marginal",
          "summaries are subgroup-specific."
        )
      )
    }
  }
  assert(
    length(facet) %in% 1L + use_subgroup,
    message = paste(
      "'facet' must have 1 element if the marginal summaries have no",
      "subgroup, but 2 unique elements if there is a subgroup."
    )
  )
  assert(
    length(unique(c(compare, axis, facet))) == 3L + use_subgroup,
    message = paste(
      "'compare', 'axis', and 'facet' must have all unique values.",
      "\"subgroup\" must be included somewhere if and only if",
      "the marginal summaries are subgroup-specific."
    )
  )
  assert(
    use_subgroup || (compare != "subgroup"),
    message = paste(
      "'compare' cannot be \"subgroup\" when the summaries have no subgroup."
    )
  )
  assert(
    use_subgroup || (axis != "subgroup"),
    message = paste(
      "'axis' cannot be \"subgroup\" when the summaries have no subgroup."
    )
  )
  subgroup <- if_any(use_subgroup, "subgroup", character(0L))
  data <- tidyr::pivot_wider(
    data = data,
    id_cols = tidyselect::any_of(c("source", "group", subgroup, "time")),
    names_from = "statistic",
    values_from = "value"
  )
  ggplot2::ggplot(data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!as.symbol(axis),
        y = mean,
        color = !!as.symbol(compare)
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = !!as.symbol(axis),
        ymin = lower,
        ymax = upper,
        color = !!as.symbol(compare)
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::ylab(marginal) +
    if_any(
      use_subgroup,
      ggplot2::facet_grid(
        as.formula(sprintf("%s ~ %s", facet[2L], facet[1L]))
      ),
      ggplot2::facet_wrap(as.formula(paste("~", facet)))
    )
}

data_compare_clean <- function(data, marginal) {
  if ("marginal" %in% colnames(data)) {
    marginal_choices <- unique(data$marginal)
    assert(
      marginal %in% marginal_choices,
      message = paste(
        "marginal argument must be one of:",
        paste(sprintf("\"%s\"", marginal_choices), collapse = ", ")
      )
    )
    data <- data[data$marginal == marginal, ]
  }
  data <- data[data$statistic %in% c("mean", "lower", "upper"), ]
  columns <- c("statistic", "group", "subgroup", "time", "value")
  data[, intersect(columns, colnames(data)), drop = FALSE]
}
