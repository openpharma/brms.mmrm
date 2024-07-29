#' @title Treatment effect archetype
#' @export
#' @family informative prior archetypes
#' @description Create an informative prior archetype for a simple treatment
#'   effect parameterization.
#' @details In this archetype, each fixed effect is either a placebo response
#'   or a treatment effect.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates. Assume group A
#'   is the reference group (e.g. placebo).
#'   Let `mu_gt` be the marginal mean of the response at group
#'   `g` time `t` given data and hyperparameters.
#'   The model has fixed effect parameters `beta_1`, `beta_2`, ..., `beta_6`
#'   which express the marginal means `mu_gt` as follows:
#'
#'       `mu_A1 = beta_1`
#'       `mu_A2 = beta_2`
#'       `mu_A3 = beta_3`
#'
#'       `mu_B1 = beta_1 + beta_4`
#'       `mu_B2 = beta_2 + beta_5`
#'       `mu_B3 = beta_3 + beta_6`
#'
#'    Above, `beta_2` is the group mean of treatment group A at time 2,
#'    and `beta_5` is the treatment effect of B relative to A at time 2.
#'
#' @inheritSection brm_archetype_successive_cells Nuisance variables
#' @inheritSection brm_prior_archetype Prior labeling
#' @section Prior labeling for [brm_archetype_effects()]:
#'   In the reference group (e.g. placebo) each fixed effect is a cell
#'   mean at a time point. In each non-reference group, each fixed effect
#'   is the treatment effect relative to the reference (at a time point).
#'   The labeling scheme in [brm_prior_label()] and
#'   [brm_prior_archetype()] translate straightforwardly. For example,
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "A", time = "2")`
#'   declares a `normal(1.2, 5)` on `beta_2` (cell mean of the reference
#'   group at time 2). Similarly,
#'   `brm_prior_label(code = "normal(1.3, 6)", group = "B", time = "2")`
#'   declares a `normal(1.3, 6)` prior on the treatment effect of group
#'   `B` relative to group `A` at discrete time point `2`.
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
#' archetype <- brm_archetype_effects(data)
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
brm_archetype_effects <- function(
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
    archetype_effects_subgroup(data, clda, prefix_interest),
    archetype_effects(data, clda, prefix_interest)
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
    subclass = "brms_mmrm_effects"
  )
}

archetype_effects <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[group]])
  levels_time <- brm_levels(data[[time]])
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
  matrix <- NULL
  names_group <- character(0L)
  names_time <- character(0L)
  for (name_group in levels_group) {
    for (name_time in levels_time) {
      if (name_group == reference_group) {
        column <- data[[time]] == name_time
      } else if (clda && (name_time == reference_time)) {
        next
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

archetype_effects_subgroup <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- brm_levels(data[[subgroup]])
  levels_time <- brm_levels(data[[time]])
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
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
        if (name_group == reference_group) {
          column <- (data[[subgroup]] == name_subgroup) &
            (data[[time]] == name_time)
        } else if (clda && (name_time == reference_time)) {
          next
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
