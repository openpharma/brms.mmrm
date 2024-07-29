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
#'   and the labeling scheme in [brm_prior_label()] and
#'   [brm_prior_archetype()] translate easily. For example,
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "B", time = "VISIT2")`
#'   declares a `normal(1.2, 5)` prior on the cell mean of treatment
#'   group `B` at discrete time point `VISIT2`.
#'   To confirm that you set the prior correctly, compare the `brms` prior
#'   with the output of `summary(your_archetype)`.
#'   See the examples for details.
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
brm_archetype_cells <- function(
  data,
  intercept = FALSE,
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline")),
  covariates = TRUE,
  clda = FALSE,
  prefix_interest = "x_",
  prefix_nuisance = "nuisance_"
) {
  assert_lgl(clda, "clda must be TRUE or FALSE")
  if (clda) {
    assert(
      !is.null(attr(data, "brm_reference_time")),
      message = "clda = TRUE requires non-NULL reference_time in brm_data()"
    )
  }
  brm_data_validate.default(data)
  data <- brm_data_remove_archetype(data)
  data <- brm_data_fill(data)
  brm_archetype_assert_prefixes(
    prefix_interest = prefix_interest,
    prefix_nuisance = prefix_nuisance
  )
  archetype <- if_any(
    brm_data_has_subgroup(data),
    archetype_cells_subgroup(data, clda, prefix_interest),
    archetype_cells(data, clda, prefix_interest)
  )
  brm_archetype_init(
    data = data,
    interest = archetype$interest,
    nuisance = nuisance,
    mapping = archetype$mapping,
    intercept = intercept,
    baseline = baseline,
    baseline_subgroup = baseline_subgroup,
    baseline_subgroup_time = baseline_subgroup_time,
    baseline_time = baseline_time,
    covariates = covariates,
    prefix_nuisance = prefix_nuisance,
    subclass = "brms_mmrm_cells"
  )
}

archetype_cells <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
  levels_group <- brm_levels(data[[group]])
  levels_time <- brm_levels(data[[time]])
  matrix <- NULL
  names_group <- character(0L)
  names_time <- character(0L)
  for (name_group in levels_group) {
    for (name_time in levels_time) {
      if (clda && (name_time == reference_time)) {
        if (name_group == reference_group) {
          column <- data[[time]] == name_time
        } else {
          next
        }
      } else {
        column <- (data[[group]] == name_group) & (data[[time]] == name_time)
      }
      matrix <- cbind(matrix, as.integer(column))
      names_group <- c(names_group, name_group)
      names_time <- c(names_time, name_time)
    }
  }
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

archetype_cells_subgroup <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- brm_levels(data[[subgroup]])
  levels_time <- brm_levels(data[[time]])
  n_group <- length(levels_group)
  n_subgroup <- length(levels_subgroup)
  n_time <- length(levels_time)
  matrix <- NULL
  names_group <- character(0L)
  names_subgroup <- character(0L)
  names_time <- character(0L)
  for (name_group in levels_group) {
    for (name_subgroup in levels_subgroup) {
      for (name_time in levels_time) {
        if (clda && (name_time == reference_time)) {
          if (name_group == reference_group) {
            column <- column <- (data[[subgroup]] == name_subgroup) &
              (data[[time]] == name_time)
          } else {
            next
          }
        } else {
          column <- (data[[group]] == name_group) &
            (data[[subgroup]] == name_subgroup) &
            (data[[time]] == name_time)
        }
        matrix <- cbind(matrix, as.integer(column))
        names_group <- c(names_group, name_group)
        names_subgroup <- c(names_subgroup, name_subgroup)
        names_time <- c(names_time, name_time)
      }
    }
  }
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
