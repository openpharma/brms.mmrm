#' @title Cell-means-like time-averaged archetype
#' @export
#' @family informative prior archetypes
#' @description Create a cell-means-like informative prior archetype
#'   with a special fixed effect to represent the average across time.
#' @details This archetype has a special fixed effect for each treatment group
#'   to represent the mean response averaged across all the time points.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates.
#'   Let `mu_gt` be the marginal mean of the response at group
#'   `g` time `t` given data and hyperparameters.
#'   The model has fixed effect parameters `beta_1`, `beta_2`, ..., `beta_6`
#'   which express the marginal means `mu_gt` as follows:
#'
#'       `mu_A1 = 3 * beta_1 - beta_2 - beta_3`
#'       `mu_A2 = beta_2`
#'       `mu_A3 = beta_3`
#'
#'       `mu_B1 = 3 * beta_4 - beta_5 - beta_6`
#'       `mu_B2 = beta_5`
#'       `mu_B3 = beta_6`
#'
#'   For group A, `beta_1` is the average response in group A
#'   averaged across time points. You can confirm this yourself
#'   by expressing the average across time
#'   `(mu_A1 + mu_A2 + mu_A3) / 3` in terms of the `beta_*` parameters
#'   and confirming that the expression simplifies down to just `beta_1`.
#'   `beta_2` represents the mean response in group A at time 2, and
#'   `beta_3` represents the mean response in group A at time 3.
#'   `beta_4`, `beta_5`, and `beta_6` are analogous for group B.
#' @inheritSection brm_archetype_successive_cells Nuisance variables
#' @inheritSection brm_prior_archetype Prior labeling
#' @section Prior labeling for [brm_archetype_average_cells()]:
#'   Within each treatment group, the initial time point represents
#'   the average, and each successive time point represents the response
#'   within that actual time.
#'   To illustrate, consider the example in the Details section.
#'   In the labeling scheme for [brm_archetype_average_cells()],
#'   you can label the prior on `beta_1` using
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "A", time = "1")`.
#'   Similarly, you cal label the prior on `beta_5` with
#'   `brm_prior_label(code = "normal(1.3, 7)", group = "B", time = "2")`.
#'   To confirm that you set the prior correctly, compare the `brms` prior
#'   with the output of `summary(your_archetype)`.
#'   See the examples for details.
#' @return A special classed `tibble` with data tailored to
#'   the cell-means-like time-averaged archetype. The dataset is augmented
#'   with extra columns with the `"archetype_"` prefix, as well as special
#'   attributes to tell downstream functions like [brm_formula()] what to
#'   do with the object.
#' @inheritParams brm_formula
#' @inheritParams brm_model
#' @inheritParams brm_archetype_successive_cells
#' @examples
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
#'   )
#' dplyr::select(
#'   data,
#'   group,
#'   time,
#'   patient,
#'   starts_with("biomarker"),
#'   starts_with("status")
#' )
#' archetype <- brm_archetype_average_cells(data)
#' archetype
#' summary(archetype)
#' formula <- brm_formula(archetype)
#' formula
#' prior <- brm_prior_label(
#'   code = "normal(1, 2.2)",
#'   group = "group_1",
#'   time = "time_2"
#' ) |>
#'   brm_prior_label("normal(1, 3.3)", group = "group_1", time = "time_3") |>
#'   brm_prior_label("normal(1, 4.4)", group = "group_1", time = "time_4") |>
#'   brm_prior_label("normal(2, 2.2)", group = "group_2", time = "time_2") |>
#'   brm_prior_label("normal(2, 3.3)", group = "group_2", time = "time_3") |>
#'   brm_prior_label("normal(2, 4.4)", group = "group_2", time = "time_4") |>
#'   brm_prior_archetype(archetype)
#' prior
#' class(prior)
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = archetype,
#'         formula = formula,
#'         prior = prior,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' suppressWarnings(print(model))
#' brms::prior_summary(model)
#' draws <- brm_marginal_draws(
#'   data = archetype,
#'   formula = formula,
#'   model = model
#' )
#' summaries_model <- brm_marginal_summaries(draws)
#' summaries_data <- brm_marginal_data(data)
#' brm_plot_compare(model = summaries_model, data = summaries_data)
#' }
brm_archetype_average_cells <- function(
  data,
  covariates = TRUE,
  prefix_interest = "x_",
  prefix_nuisance = "nuisance_",
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline"))
) {
  brm_data_validate.default(data)
  data <- brm_data_remove_archetype(data)
  data <- brm_data_fill(data)
  assert_chr(
    prefix_interest %||nzchar% "x",
    "prefix_interest must be a single character string"
  )
  assert_chr(
    prefix_nuisance %||nzchar% "x",
    "prefix_nuisance must be a single character string"
  )
  assert(
    prefix_interest != prefix_nuisance,
    message = "prefix_interest and prefix_nuisance must be different"
  )
  archetype <- if_any(
    brm_data_has_subgroup(data),
    archetype_average_cells_subgroup(data, prefix_interest),
    archetype_average_cells(data, prefix_interest)
  )
  nuisance <- archetype_nuisance(
    data = data,
    interest = archetype$interest,
    prefix = prefix_nuisance,
    covariates = covariates,
    baseline = baseline,
    baseline_subgroup = baseline_subgroup,
    baseline_subgroup_time = baseline_subgroup_time,
    baseline_time = baseline_time
  )
  brm_archetype_init(
    data = data,
    interest = archetype$interest,
    nuisance = nuisance,
    mapping = archetype$mapping,
    subclass = "brms_mmrm_average_cells"
  )
}

archetype_average_cells <- function(data, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[attr(data, "brm_group")]])
  levels_time <- brm_levels(data[[attr(data, "brm_time")]])
  n_time <- length(levels_time)
  matrix <- NULL
  for (name_group in levels_group) {
    for (name_time in levels_time) {
      if (name_time == levels_time[1L]) {
        cell <- (data[[group]] == name_group) & (data[[time]] == name_time)
        matrix <- cbind(matrix, n_time * as.integer(cell))
      } else {
        plus <- (data[[group]] == name_group) & (data[[time]] == name_time)
        minus <- (data[[group]] == name_group) &
          (data[[time]] == levels_time[1L])
        matrix <- cbind(matrix, as.integer(plus) - as.integer(minus))
      }
    }
  }
  names_group <- rep(levels_group, each = n_time)
  names_time <- rep(levels_time, times = length(levels_group))
  names <- paste0(prefix, paste(names_group, names_time, sep = "_"))
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  mapping <- tibble::tibble(
    group = names_group,
    time = names_time,
    variable = names
  )
  list(interest = interest, mapping = mapping)
}

archetype_average_cells_subgroup <- function(data, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- brm_levels(data[[subgroup]])
  levels_time <- brm_levels(data[[time]])
  n_group <- length(levels_group)
  n_subgroup <- length(levels_subgroup)
  n_time <- length(levels_time)
  matrix <- NULL
  for (name_group in levels_group) {
    for (name_subgroup in levels_subgroup) {
      for (name_time in levels_time) {
        if (name_time == levels_time[1L]) {
          cell <- (data[[group]] == name_group) &
            (data[[subgroup]] == name_subgroup) &
            (data[[time]] == name_time)
          matrix <- cbind(matrix, n_time * as.integer(cell))
        } else {
          plus <- (data[[group]] == name_group) &
            (data[[subgroup]] == name_subgroup) &
            (data[[time]] == name_time)
          minus <- (data[[group]] == name_group) &
            (data[[subgroup]] == name_subgroup) &
            (data[[time]] == levels_time[1L])
          matrix <- cbind(matrix, as.integer(plus) - as.integer(minus))
        }
      }
    }
  }
  names_group <- rep(levels_group, each = n_time * n_subgroup)
  names_subgroup <- rep(rep(levels_subgroup, times = n_group), each = n_time)
  names_time <- rep(levels_time, times = n_group * n_subgroup)
  names <- paste0(
    prefix,
    paste(names_group, names_subgroup, names_time, sep = "_")
  )
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  mapping <- tibble::tibble(
    group = names_group,
    subgroup = names_subgroup,
    time = names_time,
    variable = names
  )
  list(interest = interest, mapping = mapping)
}
