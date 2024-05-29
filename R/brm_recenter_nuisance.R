#' @title Recenter nuisance variables
#' @export
#' @family archetype utilities
#' @description Change the center of a nuisance variable of an
#'   informative prior archetype.
#' @details By "centering vector y at scalar x", we mean taking
#'   the difference `z = y - x`. If `x` is the mean, then `mean(z)` is
#'   0. Informative prior archetypes center nuisance variables
#'   at their means so the parameters can be interpreted correctly
#'   for setting informative priors. This is appropriate most of the time,
#'   but sometimes it is better to center a column at a pre-specified
#'   scientifically meaningful fixed number. If you want a nuisance column
#'   to be centered at a fixed value other than its mean,
#'   use [brm_recenter_nuisance()] to shift the center. This function
#'   can handle any nuisance variable
#' @return An informative prior archetype data frame with one of the
#'   variables re-centered.
#' @param data An informative prior archetype data frame output from
#'   [brm_archetype_cells()] or similar.
#' @param nuisance Character of length 1, name of the nuisance column
#'   in the data to shift the center.
#' @param center Numeric of length 1, value of the center to shift
#'   the column in `nuisance`. The affected column in the returned
#'   archetype data frame will look as if it were centered by this
#'   value to begin with.
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
#' archetype <- brm_archetype_cells(data)
#' mean(archetype$nuisance_biomarker1) # after original centering
#' center <- mean(data$biomarker1)
#' center # original center, before the centering from brm_archetype_cells()
#' attr(archetype$nuisance_biomarker1, "brm_center") # original center
#' max(abs((data$biomarker1 - center) - archetype$nuisance_biomarker1))
#' # Re-center nuisance_biomarker1 at 0.75.
#' archetype <- brm_recenter_nuisance(
#'   data = archetype,
#'   nuisance = "nuisance_biomarker1",
#'   center = 0.75
#' )
#' attr(archetype$nuisance_biomarker1, "brm_center") # new center
#' mean(archetype$nuisance_biomarker1) # no longer equal to the center
#' # nuisance_biomarker1 is now as though we centered it at 0.75.
#' max(abs((data$biomarker1 - 0.75) - archetype$nuisance_biomarker1))
brm_recenter_nuisance <- function(data, nuisance, center) {
  brm_data_validate(data = data)
  assert(
    inherits(data, "brms_mmrm_archetype"),
    message = "data must be a brms.mmrm informative prior archetype"
  )
  assert_chr(nuisance, "nuisance must be a character string")
  assert(
    nuisance %in% attr(data, "brm_archetype_nuisance"),
    message = c(
      "nuisance must be the name of a nuisance column in the archetype data"
    )
  )
  assert_num(center, "center must be a valid non-missing numeric scalar")
  original <- attr(data[[nuisance]], "brm_center")
  assert_num(
    original,
    paste("original center of", nuisance, "could not be found")
  )
  data[[nuisance]] <- data[[nuisance]] + original - center
  attr(data[[nuisance]], "brm_center") <- center
  data
}
