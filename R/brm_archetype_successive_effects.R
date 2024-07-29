#' @title Treatment-effect-like successive differences archetype
#' @export
#' @family informative prior archetypes
#' @description Create an informative prior archetype where the fixed effects
#'   are successive differences between adjacent time points and terms
#'   in non-reference groups are treatment effects.
#' @details Within the reference treatment group (e.g. placebo),
#'   each fixed effect is either an intercept
#'   on the first time point or the difference between two adjacent time
#'   points. In each non-reference treatment group,
#'   each model parameter is defined as an effect relative to the
#'   reference group.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates.
#'   Say group A is the reference group (e.g. placebo).
#'   Let `mu_gt` be the marginal mean of the response at group
#'   `g` time `t` given data and hyperparameters.
#'   The model has fixed effect parameters `beta_1`, `beta_2`, ..., `beta_6`
#'   which express the marginal means `mu_gt` as follows:
#'
#'       `mu_A1 = beta_1`
#'       `mu_A2 = beta_1 + beta_2`
#'       `mu_A3 = beta_1 + beta_2 + beta_3`
#'
#'       `mu_B1 = beta_1 + beta_4`
#'       `mu_B2 = beta_1 + beta_2 + beta_4 + beta_5`
#'       `mu_B3 = beta_1 + beta_2 + beta_3 + beta_4 + beta_5 + beta_6`
#'
#'   For group A, `beta_1` is the time 1 intercept, `beta_2` represents
#'   time 2 minus time 1, and `beta_3` represents time 3 minus time 2.
#'   `beta_4` is the treatment effect of group B relative to group A at
#'   time 1. `beta_5` is the treatment effect of the difference between
#'   times 2 and 1, relative to treatment group A.
#'   Similarly, `beta_6` is the treatment effect of the difference between
#'   times 3 and 2, relative to treatment group A.
#' @inheritSection brm_archetype_successive_cells Nuisance variables
#' @inheritSection brm_prior_archetype Prior labeling
#' @section Prior labeling for [brm_archetype_successive_effects()]:
#'   Within each treatment group, each intercept is labeled by the earliest
#'   time point, and each successive difference term gets the successive
#'   time point as the label.
#'   To illustrate, consider the example in the Details section.
#'   In the labeling scheme for [brm_archetype_successive_effects()],
#'   you can label the prior on `beta_1` using
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "A", time = "1")`.
#'   Similarly, you cal label the prior on `beta_5` with
#'   `brm_prior_label(code = "normal(1.3, 7)", group = "B", time = "2")`.
#'   To confirm that you set the prior correctly, compare the `brms` prior
#'   with the output of `summary(your_archetype)`.
#'   See the examples for details.
#' @return A special classed `tibble` with data tailored to
#'   the successive differences archetype. The dataset is augmented with
#'   extra columns with the `"archetype_"` prefix, as well as special
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
#' archetype <- brm_archetype_successive_effects(data)
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
brm_archetype_successive_effects <- function(
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
  brm_data_validate.default(data)
  data <- brm_data_remove_archetype(data)
  data <- brm_data_fill(data)
  brm_archetype_assert_prefixes(
    prefix_interest = prefix_interest,
    prefix_nuisance = prefix_nuisance
  )
  archetype <- if_any(
    brm_data_has_subgroup(data),
    archetype_successive_effects_subgroup(data, prefix_interest),
    archetype_successive_effects(data, prefix_interest)
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
    clda = clda,
    prefix_nuisance = prefix_nuisance,
    subclass = "brms_mmrm_successive_effects"
  )
}

archetype_successive_effects <- function(data, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[group]])
  levels_time <- brm_levels(data[[time]])
  reference <- attr(data, "brm_reference_group")
  n_time <- length(levels_time)
  data_first <- data[data[[time]] == data[[time]][1L], ]
  matrix_group <- NULL
  for (name_group in levels_group) {
    if (name_group == reference) {
      column <- rep(1L, nrow(data_first))
    } else {
      column <- data_first[[group]] == name_group
    }
    matrix_group <- cbind(matrix_group, as.integer(column))
  }
  matrix_time <- diag(n_time) + lower.tri(diag(n_time))
  matrix <- kronecker(X = matrix_group, Y = matrix_time)
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

archetype_successive_effects_subgroup <- function(data, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- brm_levels(data[[subgroup]])
  levels_time <- brm_levels(data[[time]])
  reference <- attr(data, "brm_reference_group")
  n_group <- length(levels_group)
  n_subgroup <- length(levels_subgroup)
  n_time <- length(levels_time)
  data_first <- data[data[[time]] == data[[time]][1L], ]
  matrix_group <- NULL
  for (name_group in levels_group) {
    for (name_subgroup in levels_subgroup) {
      if (name_group == reference) {
        in_group_subgroup <- (data_first[[subgroup]] == name_subgroup)
      } else {
        in_group_subgroup <- (data_first[[group]] == name_group) &
          (data_first[[subgroup]] == name_subgroup)
      }
      matrix_group <- cbind(matrix_group, as.integer(in_group_subgroup))
    }
  }
  matrix_time <- diag(n_time) + lower.tri(diag(n_time))
  matrix <- kronecker(X = matrix_group, Y = matrix_time)
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
