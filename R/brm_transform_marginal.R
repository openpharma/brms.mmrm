#' @title Marginal mean transformation
#' @export
#' @family transformations
#' @description Transformation from model parameters to marginal means.
#' @details The matrix from [brm_transform_marginal()] is passed to
#'   the `transform_marginal` argument of [brm_marginal_draws()],
#'   and it transforms posterior draws of model paramters to
#'   posterior draws of marginal means. You may customize the output of
#'   [brm_transform_marginal()] before passing it to [brm_marginal_draws()].
#'   However, please do not modify the dimensions, row names, or column
#'   names.
#' @return A matrix to transform model parameters (columns) into
#'   marginal means (rows).
#' @inheritParams brm_marginal_draws
#' @param prefix Character of length 1, prefix to add to
#'   the model matrix (`"X"`) from [brms::make_standata()] in order to
#'   reconstruct the `brms` model paramter names. This argument should
#'   only be modified for testing purposes.
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
#' transform <- brm_transform_marginal(data = data, formula = formula)
#' }
brm_transform_marginal <- function(data, formula, prefix = "b_") {
  brm_data_validate(data)
  brm_formula_validate(formula)
  data[[attr(data, "brm_outcome")]] <- 0
  grid <- transform_marginal_grid(data = data)
  grid <- dplyr::bind_cols(grid, transform_marginal_continuous(data))
  formula_transform <- transform_marginal_formula(data, formula = formula)
  model_matrix <- brms::make_standata(data = data, formula = formula)$X
  transform <- brms::make_standata(formula = formula_transform, data = grid)$X
  transform <- dplyr::bind_cols(transform, transform_marginal_discrete(data))
  transform <- as.matrix(transform)[, colnames(model_matrix)]
  colnames(transform) <- paste0(prefix, colnames(transform))
  rownames(transform) <- brm_transform_marginal_names_rows(
    data = data,
    formula = formula,
    grid = grid
  )
  transform
}

brm_transform_marginal_names_rows <- function(data, formula, grid) {
  has_subgroup <- brm_formula_has_subgroup(formula)
  group <- grid[[attr(data, "brm_group")]]
  subgroup <- if_any(has_subgroup, grid[[attr(data, "brm_subgroup")]], NULL)
  time <- grid[[attr(data, "brm_time")]]
  if_any(
    has_subgroup,
    name_marginal_subgroup(group = group, subgroup = subgroup, time = time),
    name_marginal(group = group, time = time)
  )
}

transform_marginal_grid <- function(data) {
  args <- lapply(c("brm_group", "brm_subgroup", "brm_time"), attr, x = data)
  args <- lapply(unlist(args), as.symbol)
  args$.data <- data
  do.call(what = dplyr::distinct, args = args)
}

transform_marginal_continuous <- function(data) {
  t(colMeans(data[, transform_marginal_names_continuous(data)]))
}

transform_marginal_discrete <- function(data) {
  discrete <- transform_marginal_names_discrete(data)
  terms <- paste(c("~ 0", discrete), collapse = " + ")
  base_formula <- as.formula(paste(attr(data, "brm_outcome"), terms))
  brms_formula <- brms::brmsformula(base_formula)
  model_matrix <- brms::make_standata(formula = brms_formula, data = data)$X
  t(colMeans(model_matrix))
}

transform_marginal_formula <- function(data, formula) {
  discrete <- transform_marginal_names_discrete(data)
  for (name in discrete) {
    data[[name]] <- NULL
  }
  attr(data, "brm_covariates") <- setdiff(
    attr(data, "brm_covariates"),
    discrete
  )
  args <- attributes(formula)
  args <- args[grepl(pattern = "^brm_", x = names(args))]
  names(args) <- gsub(pattern = "^brm_", replacement = "", x = names(args))
  args$data <- data
  args$correlation <- "diagonal"
  do.call(what = brm_formula, args = args)
}

transform_marginal_names_continuous <- function(data) {
  choices <- c(
    attr(data, "brm_outcome"),
    attr(data, "brm_baseline"),
    attr(data, "brm_covariates")
  )
  intersect(names(Filter(is.numeric, data)), choices)
}

transform_marginal_names_discrete <- function(data) {
  choices <- c(attr(data, "brm_covariates"))
  Filter(function(name) !is.numeric(data[[name]]), choices)
}
