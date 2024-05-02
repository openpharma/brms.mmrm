#' @title 
#' @section Prior labeling:
#'   Informative prior archetypes use a labeling scheme to assign
#'   priors. Priors are labeled using levels in the data, and then
#'   [brm_prior_archetype()] uses the labels to map each prior to its
#'   corresponding parameter in the model. This makes it possible
#'   to assign informative priors without having to guess the names
#'   of specific model coefficients in `brms`. The specific mapping is
#'   varied from archetype to archetype, and it is explained in
#'   specific archetype functions such as [brm_archetype_successive_cells()].
#' @examples
#' set.seed(0L)
#' data <- brm_simulate_outline(
#'   n_group = 2,
#'   n_patient = 100,
#'   n_time = 3,
#'   rate_dropout = 0,
#'   rate_lapse = 0
#' ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n())) |>
#'   brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
#'   brm_simulate_categorical(
#'     names = c("status1", "status2"),
#'     levels = c("present", "absent")
#'   )
#' archetype <- brm_archetype_successive_cells(data)
#' dplyr::distinct(data, group, time)
#' label <- brm_prior_label(
#'   code = "normal(1, 1)",
#'   group = "group_1",
#'   time = "time_1"
#' ) |>
#'   brm_prior_label("normal(1, 2)", group = "group_1", time = "time_2") |>
#'   brm_prior_label("normal(1, 3)", group = "group_1", time = "time_3") |>
#'   brm_prior_label("normal(2, 1)", group = "group_2", time = "time_1") |>
#'   brm_prior_label("normal(2, 2)", group = "group_2", time = "time_2") |>
#'   brm_prior_label("normal(2, 3)", group = "group_2", time = "time_3")
#' label
brm_prior_archetype <- function(archetype, label) {
  brm_data_validate(archetype)
  assert(
    inherits(archetype, "brms_mmrm_archetype"),
    message = "archetype must be an informative prior archetype"
  )
  param <- attr(archetype, "brm_archetype_mapping")
  
}

#' @title Label a prior with levels in the data.
#' @export
#' @family priors
#' @description Label an informative prior for a parameter
#'   using a collection of levels in the data.
#' @details [brm_prior_archetype()] builds a `brms` prior for an
#'   informative prior archetype model. Successive calls to
#'   [brm_prior_parameter()] build the prior specificiation for the `label`
#'   argument of [brm_prior_archetype()].
#' @inheritSection brm_prior_archetype Prior labeling
#' @return A `tibble` with one row per model parameter and columns for the
#'   Stan code, treatment group, subgroup, and discrete time point
#'   of each parameter.
#' @param label A `tibble` with the prior labeling scheme so far,
#'   with one row per model parameter and columns for the
#'   Stan code, treatment group, subgroup, and discrete time point
#'   of each parameter.
#' @param code Character of length 1, Stan code for the prior. Could be
#'   a string like `"normal(1, 2.2)"`. The full set of priors is given
#'   in the Stan Reference Manual, available from <https://mc-stan.org/>.
#'   See the documentation [brms::set_prior()] for more details.
#' @param group Value of length 1, level of the treatment group column
#'   in the data to label the prior. The treatment group column
#'   is the one you identified with the `group` argument of [brm_data()].
#' @param subgroup Value of length 1, level of the subgroiup column
#'   in the data to label the prior. The subgroup column
#'   is the one you identified with the `subgroup` argument of [brm_data()].
#'   Not every dataset has a subgroup. If yours does not, please use
#'   the default value `NA`.
#' @param time Value of length 1, level of the discrete time column
#'   in the data to label the prior. The discrete time column
#'   is the one you identified with the `time` argument of [brm_data()].
#' @examples
#' set.seed(0L)
#' data <- brm_simulate_outline(
#'   n_group = 2,
#'   n_patient = 100,
#'   n_time = 3,
#'   rate_dropout = 0,
#'   rate_lapse = 0
#' ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n())) |>
#'   brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
#'   brm_simulate_categorical(
#'     names = c("status1", "status2"),
#'     levels = c("present", "absent")
#'   )
#' archetype <- brm_archetype_successive_cells(data)
#' dplyr::distinct(data, group, time)
#' label <- brm_prior_label(
#'   code = "normal(1, 1)",
#'   group = "group_1",
#'   time = "time_1"
#' ) |>
#'   brm_prior_label("normal(1, 2)", group = "group_1", time = "time_2") |>
#'   brm_prior_label("normal(1, 3)", group = "group_1", time = "time_3") |>
#'   brm_prior_label("normal(2, 1)", group = "group_2", time = "time_1") |>
#'   brm_prior_label("normal(2, 2)", group = "group_2", time = "time_2") |>
#'   brm_prior_label("normal(2, 3)", group = "group_2", time = "time_3")
#' label
brm_prior_label <- function(
  label = NULL,
  code = NA_character_,
  group = NA,
  subgroup = NA,
  time = NA
) {
  assert(
    is.null(label) || is.data.frame(label),
    message = "label must be NULL or a data frame"
  )
  assert_chr(code, "code must be a character string")
  assert(length(group) == 1L, message = "group must have length 1")
  assert(length(subgroup) == 1L, message = "subgroup must have length 1")
  assert(length(time) == 1L, message = "time must have length 1")
  dplyr::bind_rows(
    label,
    tibble::tibble(
      code = code,
      group = group,
      subgroup = subgroup,
      time = time
    )
  )
}
