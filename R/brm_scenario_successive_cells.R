#' @title Cell-means-like successive differences scenario
#' @export
#' @family informative prior scenarios
#' @description Create an informative prior scenario where the fixed effects
#'   are successive differences between adjacent time points.
#' @details In this parameterization, each fixed effect is either an intercept
#'   on the first time point or the difference between two adjacent time
#'   points, and each treatment group has its own set of fixed effects
#'   independent of the other treatment groups.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates.
#'   Let `mu_gt` be the conditional expectation of the response at group
#'   `g` time `t` given data and hyperparameters. Then, the cell-means-like
#'   successive differences parameterization declares model coefficients
#'   `beta_1`, `beta_2`, ... `beta_6` as follows:
#'
#'       `mu_A1 = beta_1`
#'       `mu_A2 = beta_1 + beta_2`
#'       `mu_A3 = beta_1 + beta_2 + beta_3`
#'
#'       `mu_B1 = beta_4`
#'       `mu_B2 = beta_4 + beta_5`
#'       `mu_B3 = beta_4 + beta_5 + beta_6`
#'
#'   For group A, `beta_1` is the time 1 intercept, `beta_2` represents
#'   time 2 minus time 1, and `beta_3` represents time 3 minus time 2.
#'   `beta_4`, `beta_5`, and `beta_6` represent the analogous roles.
#' @section Nuisance variables in informative prior scenarios:
#'   In the presence of other covariates, functions like
#'   [brm_scenario_successive()] convert nuisance factors into binary
#'   dummy variables, then center all those dummy variables and any
#'   continuous nuisance variables at their means in the data.
#'   This ensures that the main model coefficients
#'   of interest are not implicitly conditional on a subset of the data.
#'   In other words, preprocessing nuisance variables this way preserves
#'   the interpretations of the fixed effects of interest, and it ensures
#'   informative priors can be specified correctly.
#' @return A special classed `tibble` with data tailored to
#'   the successive differences scenario. The dataset is augmented with
#'   extra columns with the `"scenario_"` prefix, as well as special
#'   attributes to tell downstream functions like [brm_formula()] what to
#'   do with the object.
#' @inheritParams brm_model
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_simulate_outline(
#'   n_group = 2,
#'   n_patient = 100,
#'   n_time = 4,
#'   rate_dropout = 0,
#'   rate_lapse = 0
#' ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n())) |>
#'   brm_data_change() |>
#'   brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
#'   brm_simulate_categorical(
#'     names = c("status1", "status2"),
#'     levels = c("present", "absent")
#'   ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n()))
#' dplyr::select(
#'   data,
#'   group,
#'   time,
#'   patient,
#'   starts_with("biomarker"),
#'   starts_with("status")
#' )
#' brm_scenario_successive_cells(data)
#' }
brm_scenario_successive_cells <- function(data) {
  brm_data_validate(data)
  data <- brm_data_fill(data)
  scenario <- scenario_successive_cells(data)
  nuisance <- scenario_nuisance(data)
  out <- brm_scenario_init(
    data = data,
    interest = scenario$interest,
    nuisance = nuisance,
    parameterization = scenario$parameterization,
    subclass = "brms_mmrm_successive_cells"
  )
}

scenario_successive_cells <- function(data) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  levels_group <- attr(data, "brm_levels_group")
  levels_time <- attr(data, "brm_levels_time")
  n_time <- length(levels_time)
  data_first <- data[data[[time]] == data[[time]][1L], ]
  matrix_group <- NULL
  for (name in levels_group) {
    matrix_group <- cbind(
      matrix_group,
      as.integer(data_first[[group]] == name)
    )
  }
  matrix_time <- diag(n_time) + lower.tri(diag(n_time))
  matrix <- kronecker(X = matrix_group, Y = matrix_time)
  names_group <- rep(levels_group, each = n_time)
  names_time <- rep(levels_time, times = length(levels_group))
  names <- paste("interest", group, names_group, time, names_time, sep = "_")
  names <- brm_levels(names)
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  parameterization <- tibble::tibble(
    group = names_group,
    time = names_time,
    variable = names
  )
  list(interest = interest, parameterization = parameterization)
}
