#' @title Append simulated categorical covariates
#' @export
#' @family simulation
#' @description Simulate and append non-time-varying
#'   categorical covariates to an existing [brm_data()] dataset.
#' @details Each covariate is a new column of the dataset with one independent
#'   random categorical draw for each patient, using a fixed set of levels
#'   (via `base::sample()` with `replace = TRUE`).
#'   All covariates simulated this way are
#'   independent of everything else in the data, including other covariates
#'   (to the extent that the random number generators in R work as intended).
#' @return A classed `tibble`, like from [brm_data()] or
#'   [brm_simulate_outline()], but with new categorical covariate columns
#'   and with the names of the new covariates appended to the
#'   `brm_covariates` attribute. Each new categorical covariate column
#'   is a character vector, not the factor type in base R.
#' @inheritParams brm_simulate_continuous
#' @param levels Character vector of unique levels of the
#'   simulated categorical covariates.
#' @param probabilities Either `NULL` or a
#'  numeric vector of length `length(levels)` with levels between 0 and 1
#'  where all elements sum to 1.
#'  If `NULL`, then all levels are equally likely to be drawn. If not `NULL`,
#'  then `probabilities` is a vector of sampling probabilities corresponding
#'  to each respective level of `levels`.
#' @examples
#' data <- brm_simulate_outline()
#' brm_simulate_categorical(
#'   data = data,
#'   names = c("site", "region"),
#'   levels = c("area1", "area2")
#' )
#' brm_simulate_categorical(
#'   data = data,
#'   names = c("site", "region"),
#'   levels = c("area1", "area2"),
#'   probabilities = c(0.1, 0.9)
#' )
brm_simulate_categorical <- function(
  data,
  names,
  levels,
  probabilities = NULL
) {
  brm_data_validate(data)
  assert_chr_vec(names, message = "names must be a valid character vector")
  assert_pos(length(names), message = "names must not be empty")
  assert_machine_names(names)
  assert(!anyDuplicated(names), message = "names must all be unique")
  assert(
    !any(names %in% colnames(data)),
    message = paste(
      "columns already in data:",
      paste(intersect(names, colnames(data)), collapse = ", ")
    )
  )
  assert_chr_vec(levels, message = "levels must be a valid character vector")
  assert_pos(length(levels), message = "levels must not be empty")
  assert_machine_names(levels)
  assert(!anyDuplicated(levels), message = "levels must all be unique")
  if (is.null(probabilities)) {
    probabilities <- rep(1 / length(levels), length(levels))
  }
  assert(
    probabilities,
    is.numeric(.),
    !anyNA(.),
    . >= 0,
    . <= 1,
    sum(.) == 1,
    message = paste(
      "probabilties must be NULL or a valid vector of",
      "expected sampling proportions with length equal to that of levels."
    )
  )
  patient <- attr(data, "brm_patient")
  data_patient <- tibble::tibble(name = unique(data[[patient]]))
  colnames(data_patient) <- patient
  for (name in names) {
    data_patient[[name]] <- sample(
      x = levels,
      size = nrow(data_patient),
      replace = TRUE,
      prob = probabilities
    )
  }
  data <- dplyr::left_join(x = data, y = data_patient, by = patient)
  attr(data, "brm_covariates") <- union(
    x = attr(data, "brm_covariates"),
    y = names
  )
  brm_data_validate(data)
  data
}
