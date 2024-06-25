#' @title Label template for informative prior archetypes
#' @export
#' @family priors
#' @description Template for the `label` argument of [brm_prior_archetype()].
#' @details The `label` argument of [brm_prior_archetype()] is a
#'   `tibble` which maps Stan code for univariate priors
#'   to fixed effect parameters in the model. Usually this `tibble` is
#'   built gradually using multiple calls to [brm_prior_label()],
#'   but occasionally it is more convenient to begin with a full template
#'   and manually write Stan code in the `code` column.
#'   [brm_prior_template()] creates this template.
#' @inheritSection brm_prior_archetype Prior labeling
#' @return A `tibble` with one row per fixed effect parameter and columns
#'   to map Stan code to each parameter. After manually writing Stan code in
#'   the `code` column of the template, you can supply the result
#'   to the `label` argument of [brm_prior_archetype()] to build a
#'   `brms` prior for your model.
#' @inheritParams brm_prior_archetype
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
#' label <- brm_prior_template(archetype)
#' label$code <- c(
#'   "normal(1, 1)",
#'   "normal(1, 2)",
#'   "normal(1, 3)",
#'   "normal(2, 1)",
#'   "normal(2, 2)",
#'   "normal(2, 3)"
#' )
#' brm_prior_archetype(label = label, archetype = archetype)
brm_prior_template <- function(archetype) {
  brm_data_validate(archetype)
  assert(
    inherits(archetype, "brms_mmrm_archetype"),
    message = "archetype must be an informative prior archetype"
  )
  out <- attr(archetype, "brm_archetype_mapping")
  out$code <- rep(NA_character_, nrow(out))
  keep <- intersect(c("code", "group", "subgroup", "time"), colnames(out))
  out[, keep]
}
