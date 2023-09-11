#' @title Append simulated continuous covariates
#' @export
#' @family simulation
#' @description Simulate and append continuous covariates to an existing
#'   [brm_data()] dataset.
#' @details Each covariate is a new column of the dataset with independent
#'   normal draws. All covariates simulated this way are independent of other
#'   covariates (to the extent that the random number generators in R
#'   work as intended).
#' @return A classed `tibble`, like from [brm_data()] or
#'   [brm_simulate_outline()], but with new numeric covariate columns
#'   and with the names of the new covariates appended to the
#'   `brm_covariates` attribute.
#' @param data Classed `tibble` as from [brm_data()]
#'   or [brm_simulate_outline()].
#' @param names Character vector with the names of the new covariates
#'   to simulate and append. Must not already be column names of `data`.
#' @param mean Numeric of length 1,
#'   mean of the normal distribution for simulating each covariate.
#' @param sd Positive numeric of length 1,
#'   standard deviation of the normal distribution
#'   for simulating each covariate.
#' @examples
#' data <- brm_simulate_outline()
#' brm_simulate_covariates_continuous(data = data, names = c("a", "b"))
#' brm_simulate_covariates_continuous(
#'   data = data,
#'   names = c("x", "y"),
#'   mean = 1000,
#'   sd = 100
#' )
brm_simulate_covariates_continuous <- function(
  data,
  names = character(0L),
  mean = 0,
  sd = 1
) {
  brm_data_validate(data)
  assert_chr_vec(names, message = "names must be a valid character vector")
  assert_pos(length(names), message = "names must not be empty")
  assert_machine_names(names)
  assert(
    !any(names %in% colnames(data)),
    message = paste(
      "columns already in data:",
      paste(intersect(names, colnames(data)), collapse = ", ")
    )
  )
  assert_num(mean, message = "mean must be a valid number")
  assert_pos(sd, message = "sd must be a valid positive number")
  for (name in names) {
    data[[name]] <- stats::rnorm(n = nrow(data), mean = mean, sd = sd)
  }
  attr(data, "brm_covariates") <- union(
    x = attr(data, "brm_covariates"),
    y = names
  )
  data
}
