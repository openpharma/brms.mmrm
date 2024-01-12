#' @title Convert to change from baseline.
#' @export
#' @family data
#' @description Convert a dataset from raw response to change from baseline.
#' @return A classed `tibble` with change from baseline as the outcome variable
#'   and the internal attributes modified accordingly. A special baseline
#'   column is also created, and the original raw response column is removed.
#'   The new baseline column is comprised of the elements of the response
#'   variable corresponding to the `reference_time` argument of [brm_data()].
#' @param data A classed `tibble` (e.g. from [brm_data()]) with raw response
#'   as the outcome variable (role = `"response"` in [brm_data()]).
#' @param name_change Character of length 1, name of the new outcome column
#'   for change from baseline.
#' @param name_baseline Character of length 1, name of the new column for
#'   the original baseline response.
#' @examples
#' set.seed(0)
#' data <- brm_data(
#'   data = dplyr::rename(brm_simulate_simple()$data, y_values = response),
#'   outcome = "y_values",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
#' data
#' attr(data, "brm_role")
#' attr(data, "brm_outcome")
#' attr(data, "brm_baseline")
#' attr(data, "brm_reference_time")
#' changed <- brm_data_change(data = data, name_change = "delta")
#' changed
#' attr(changed, "brm_role")
#' attr(changed, "brm_outcome")
#' attr(changed, "brm_baseline")
#' attr(data, "brm_reference_time")
brm_data_change <- function(
  data,
  name_change = "change",
  name_baseline = "baseline"
) {
  brm_data_validate(data)
  assert(
    attr(data, "brm_role") == "response",
    message = paste(
      "outcome variable must be raw response",
      "(not change from baseline)",
      "in the data supplied to brm_data_change()."
    )
  )
  assert_chr(name_change)
  assert_chr(name_baseline)
  assert(
    !any(c(name_change, name_baseline) %in% colnames(data)),
    message = paste(
      "name_change and name_baseline must",
      "not already be columns in the data.",
      "Choose different values for these arguments of brm_data_change()."
    )
  )
  name_time <- attr(data, "brm_time")
  name_missing <- attr(data, "brm_missing")
  reference_time <- attr(data, "brm_reference_time")
  name_response <- attr(data, "brm_outcome")
  data_baseline <- data[data[[name_time]] == reference_time, ]
  if (!is.null(name_missing)) {
    data_baseline[[name_missing]] <- NULL
  }
  data_baseline[[name_baseline]] <- data_baseline[[name_response]]
  data_baseline[[name_response]] <- NULL
  data_baseline[[name_time]] <- NULL
  data_after <- data[data[[name_time]] != reference_time, ]
  data_after[[name_change]] <- data_after[[name_response]]
  data_after[[name_response]] <- NULL
  out <- dplyr::left_join(
    x = data_after,
    y = data_baseline,
    by = intersect(colnames(data_after), colnames(data_baseline))
  )
  out[[name_change]] <- out[[name_change]] - out[[name_baseline]]
  brm_data(
    data = out,
    outcome = name_change,
    role = "change",
    baseline = name_baseline,
    group = attr(data, "brm_group"),
    subgroup = attr(data, "brm_subgroup"),
    time = name_time,
    patient = attr(data, "brm_patient"),
    covariates = attr(data, "brm_covariates"),
    missing = attr(data, "brm_missing"),
    reference_group = attr(data, "brm_reference_group"),
    reference_subgroup = attr(data, "brm_reference_subgroup"),
    reference_time = NULL
  )
}
