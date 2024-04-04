#' @title Marginal mean transformation
#' @export
#' @family transformations
#' @description Transformation from model parameters to marginal means.
#' @details The matrix from [brm_transform_marginal()] is passed to
#'   the `transform_marginal` argument of [brm_marginal_draws()],
#'   and it transforms posterior draws of model parameters to
#'   posterior draws of marginal means. You may customize the output of
#'   [brm_transform_marginal()] before passing it to [brm_marginal_draws()].
#'   However, please do not modify the dimensions, row names, or column
#'   names.
#' @return A matrix to transform model parameters (columns) into
#'   marginal means (rows).
#' @inheritParams brm_marginal_draws

#' @param prefix Character of length 1, prefix to add to
#'   the model matrix (`"X"`) from [brms::make_standata()] in order to
#'   reconstruct the `brms` model parameter names. This argument should
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
brm_transform_marginal <- function(
  data,
  formula,
  prefix = "b_",
  average_within_subgroup = NULL
) {
  brm_data_validate(data)
  brm_formula_validate(formula)
  assert(
    prefix,
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    message = "prefix must be a nonempty non-missing character of length 1"
  )
  assert(
    average_within_subgroup,
    is.null(.) || isTRUE(.) || isFALSE(.),
    message = "average_within_subgroup must be NULL or TRUE or FALSE"
  )
  if (is.null(average_within_subgroup)) {
    average_within_subgroup <- FALSE
    if (brm_formula_has_subgroup(formula)) {
      brm_message_session(
        "In brm_transform_marginal(), the formula specifies a subgroup and ",
        "average_within_subgroup is NULL. Please read the subgroup section ",
        "of https://openpharma.github.io/brms.mmrm/articles/inference.html ",
        "and choose a logical (TRUE or FALSE) value of ",
        "average_within_subgroup appropriate to your situation.",
        id = "average_within_subgroup"
      )
    }
  }
  if (!brm_formula_has_subgroup(formula)) {
    average_within_subgroup <- FALSE
  }
  data <- brm_data_fill(data)
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  grid <- transform_marginal_grid(data = data)
  grid <- transform_marginal_continuous(
    data = data,
    grid = grid,
    average_within_subgroup = average_within_subgroup
  )
  formula_transform <- transform_marginal_formula(data, formula = formula)
  transform <- brms::make_standata(data = grid, formula = formula_transform)$X
  transform <- transform_marginal_discrete(
    data = data,
    grid = grid,
    transform = transform,
    average_within_subgroup = average_within_subgroup
  )
  names <- colnames(brms::make_standata(data = data, formula = formula)$X)
  transform <- as.matrix(transform)[, names, drop = FALSE]
  colnames(transform) <- paste0(prefix, colnames(transform))
  rownames(transform) <- brm_transform_marginal_names_rows(
    data = data,
    formula = formula,
    grid = grid
  )
  transform
}

transform_marginal_grid <- function(data) {
  args <- lapply(c("brm_group", "brm_subgroup", "brm_time"), attr, x = data)
  args <- lapply(unlist(args), as.symbol)
  args$.data <- data
  grid <- do.call(what = dplyr::distinct, args = args)
  args$.data <- grid
  do.call(what = dplyr::arrange, args = args)
}

transform_marginal_continuous <- function(
    data,
    grid,
    average_within_subgroup
) {
  subgroup <- attr(data, "brm_subgroup")
  names <- transform_marginal_names_continuous(data)
  for (name in names) {
    if (average_within_subgroup) {
      means <- tapply(X = data[[name]], INDEX = data[[subgroup]], FUN = mean)
      grid[[name]] <- unname(means[grid[[subgroup]]])
    } else {
      grid[[name]] <- mean(data[[name]])
    }
  }
  grid
}

transform_marginal_discrete <- function(
    data,
    grid,
    transform,
    average_within_subgroup
) {
  subgroup <- attr(data, "brm_subgroup")
  discrete <- transform_marginal_names_discrete(data)
  transform <- as.data.frame(transform)
  text <- paste(c("~ 0", discrete), collapse = " + ")
  base_formula <- as.formula(paste(attr(data, "brm_outcome"), text))
  brms_formula <- brms::brmsformula(base_formula)
  indicators <- brms::make_standata(data = data, formula = brms_formula)$X
  for (name in colnames(indicators)) {
    if (average_within_subgroup) {
      means <- tapply(indicators[, name], data[[subgroup]], FUN = mean)
      transform[[name]] <- unname(means[grid[[subgroup]]])
    } else {
      transform[[name]] <- mean(indicators[, name])
    }
  }
  as.matrix(transform)
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
