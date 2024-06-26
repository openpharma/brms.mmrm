#' @title Marginal names grid.
#' @export
#' @family marginals
#' @description Describe the column names of the data frames output
#'   by [brm_marginal_draws()].
#' @details Useful for creating custom posterior summaries from the draws.
#' @return A data frame with a `name` column with the names of columns of
#'   data frames in [brm_marginal_draws()], along with metadata to
#'   describe which groups, subgroups, and time points those columns
#'   correspond to.
#' @inheritParams brm_transform_marginal
#' @examples
#' data <- brm_simulate_outline()
#' brm_marginal_grid(data, brm_formula(data))
#' data <- brm_simulate_outline(n_subgroup = 2L)
#' brm_marginal_grid(data, brm_formula(data))
brm_marginal_grid <- function(data, formula) {
  brm_data_validate(data)
  brm_formula_validate(formula)
  group <- data[[attr(data, "brm_group")]]
  time <- data[[attr(data, "brm_time")]]
  if (brm_has_subgroup(data = data, formula = formula)) {
    subgroup <- data[[attr(data, "brm_subgroup")]]
    grid <- tibble::tibble(
      name = name_marginal_subgroup(
        group = group,
        subgroup = subgroup,
        time = time
      ),
      group = group,
      subgroup = subgroup,
      time = time
    )
    grid <- dplyr::arrange(grid, group, subgroup, time)
  } else {
    grid <- tibble::tibble(
      name = name_marginal(
        group = group,
        time = time
      ),
      group = group,
      time = time
    )
    grid <- dplyr::arrange(grid, group, time)
  }
  dplyr::distinct(grid)
}
