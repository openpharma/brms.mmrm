#' @title Cell means archetype
#' @export
#' @family informative prior archetypes
#' @description Create an informative prior archetype for cell means.
#' @details In this archetype, each fixed effect is a cell mean: the group
#'   mean for a given value of treatment group and discrete time
#'   (and subgroup level, if applicable).
#' @inheritSection brm_archetype_successive_cells Nuisance variables
#' @inheritSection brm_prior_archetype Prior labeling
#' @section Prior labeling for [brm_archetype_cells()]:
#'   Within each treatment group, each model parameter is a cell mean,
#'   and the labeling scheme in [brm_prior_label()] and [prior_archetype()]
#'   translate easily. For example,
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "B", time = "VISIT2")`
#'   declares a `normal(1.2, 5)` prior on the cell mean of treatment
#'   group `B` at discrete time point `VISIT2`.
#' @return A special classed `tibble` with data tailored to
#'   the successive differences archetype. The dataset is augmented with
#'   extra columns with the `"archetype_"` prefix, as well as special
#'   attributes to tell downstream functions like [brm_formula()] what to
#'   do with the object.
#' @inheritParams brm_archetype_successive_cells
#' @inheritParams brm_model
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
#' archetype <- brm_archetype_cells(data)
#' archetype
#' summary(archetype)
#' formula <- brm_formula(archetype)
#' formula
#' label <- brm_prior_label(
#'   code = "normal(1, 2.2)",
#'   group = "group_1",
#'   time = "time_2"
#' ) |>
#'   brm_prior_label("normal(1, 3.3)", group = "group_1", time = "time_3") |>
#'   brm_prior_label("normal(1, 4.4)", group = "group_1", time = "time_4") |>
#'   brm_prior_label("normal(2, 2.2)", group = "group_2", time = "time_2") |>
#'   brm_prior_label("normal(2, 3.3)", group = "group_2", time = "time_3") |>
#'   brm_prior_label("normal(2, 4.4)", group = "group_2", time = "time_4")
#' label
#' prior <- brm_prior_archetype(archetype, label)
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
brm_archetype_cells <- function(
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
    archetype_cells_subgroup(data, prefix_interest),
    archetype_cells(data, prefix_interest)
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
    subclass = "brms_mmrm_cells"
  )
}

archetype_cells <- function(data, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  levels_group <- attr(data, "brm_levels_group")
  levels_time <- attr(data, "brm_levels_time")
  matrix <- NULL
  for (name_group in levels_group) {
    for (name_time in levels_time) {
      column <- (data[[group]] == name_group) & (data[[time]] == name_time)
      matrix <- cbind(matrix, as.integer(column))
    }
  }
  names_group <- rep(levels_group, each = length(levels_time))
  names_time <- rep(levels_time, times = length(levels_group))
  names <- paste0(prefix, paste(names_group, names_time, sep = "_"))
  names <- brm_levels(names)
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  mapping <- tibble::tibble(
    group = names_group,
    time = names_time,
    variable = names
  )
  list(interest = interest, mapping = mapping)
}

archetype_cells_subgroup <- function(data, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  levels_group <- attr(data, "brm_levels_group")
  levels_subgroup <- attr(data, "brm_levels_subgroup")
  levels_time <- attr(data, "brm_levels_time")
  n_group <- length(levels_group)
  n_subgroup <- length(levels_subgroup)
  n_time <- length(levels_time)
  matrix <- NULL
  for (name_group in levels_group) {
    for (name_subgroup in levels_subgroup) {
      for (name_time in levels_time) {
        column <- (data[[group]] == name_group) &
          (data[[subgroup]] == name_subgroup) &
          (data[[time]] == name_time)
        matrix <- cbind(matrix, as.integer(column))
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
  names <- brm_levels(names)
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
