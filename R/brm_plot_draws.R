#' @title Visualize posterior draws of marginals.
#' @export
#' @family visualization
#' @description Visualize posterior draws of marginals.
#' @return A `ggplot` object.
#' @param draws A data frame of draws from an element of
#'   the output list of [brm_marginal_summaries()].
#' @param versus Character of length 1 identifying the quantity to put
#'   on the horizontal axis. Must be be one of `"time"` or `"group"` if
#'   the marginal summaries are not subgroup-specific. If the marginals
#'   are subgroup-specific, then `versus` must be one of
#'   `"time"`, `"group"`, or `"subgroup"`.
#' @param facet Character vector of length 1 or 2 with quantities to
#'   generate facets. Each element must be `"time"`, `"group"`,
#'   or `"subgroup"`, and `c(versus, facet)` must all have unique elements.
#'   `"subgroup"` is automatically removed if the marginals have no
#'   subgroup. If `facet` has length 1, then faceting is wrapped.
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
#' brm_plot_draws(draws = draws$difference_time)
#' }
brm_plot_draws <- function(
  draws,
  versus = "time",
  facet = c("group", "subgroup")
) {
  assert(is.data.frame(draws), message = "draws argument must be a data frame.")
  assert_chr(versus, "'versus' must be a single nonempty character string")
  assert_chr_vec(
    facet,
    "'facet' must be a nonempty character vector with 1 or 2 elements"
  )
  assert(
    versus %in% c("time", "group", "subgroup"),
    message = "'versus' must be \"time\", \"group\", or \"subgroup\""
  )
  assert(
    facet %in% c("time", "group", "subgroup"),
    message = "each 'facet' must be in \"time\", \"group\", or \"subgroup\""
  )
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
  } else {
    facet <- setdiff(facet, "subgroup")
  }
  assert(
    length(facet) %in% 1L + use_subgroup,
    message = paste(
      "'facet' must have 1 element if the marginal summaries have no",
      "subgroup, but 2 unique elements if there is a subgroup."
    )
  )
  assert(
    length(unique(c(versus, facet))) == 2L + use_subgroup,
    message = paste(
      "'versus' and 'facet' must include \"time\" and \"group\", as well as",
      "\"subgroup\" if the marginal summaries have a subgroup."
    )
  )
  assert(
    use_subgroup || (versus != "subgroup"),
    message = paste(
      "'versus' cannot be \"subgroup\" when the summaries have no subgroup."
    )
  )
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
      ggplot2::aes(x = value, y = !!as.symbol(versus)),
      scale = 0.9,
      stat = "binline",
      bins = 20
    ) +
    ggplot2::coord_flip() +
    if_any(
      use_subgroup,
      ggplot2::facet_grid(
        as.formula(sprintf("%s ~ %s", facet[2L], facet[1L]))
      ),
      ggplot2::facet_wrap(as.formula(paste("~", facet)))
    )
}
