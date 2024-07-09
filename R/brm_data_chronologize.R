#' @title Chronologize a dataset
#' @export
#' @family data
#' @description Convert the discrete time variable into an ordered factor.
#' @details Most MMRMs should use an ordered factor for the `time` column
#'   in the data. This way, individual time points are treated as
#'   distinct factor levels for the purposes of fixed effect parameterizations
#'   (see the "Contrasts" section), and the explicit ordering ensures
#'   that informative prior archetypes and ARMA-like correlation structures
#'   are expressed correctly. Without the ordering, problems can arise when
#'   character vectors are sorted: e.g. if `AVISIT` has levels
#'   `"VISIT1", "VISIT2", ..., "VISIT10"`, then `brms` will mistake the
#'   the order of scheduled study visits to be
#'   `"VISIT1", "VISIT10", "VISIT2", ...`, which is not chronological.
#'
#'   You can easily turn
#'   the time variable into an ordered factor using
#'   [brm_data_chronologize()]. Either supply an explicit character vector
#'   of chronologically-ordered factor levels in the `levels` argument,
#'   or supply the name of a time-ordered variable in the `order` argument.
#'
#'   [brm_data_chronologize()] can be called either before or just after
#'   [brm_data()], but in the former case, the discrete time variable
#'   needs to be specified explicitly in `time` argument. And in the latter,
#'   [brm_data_chronologize()] must be called before any of the informative
#'   prior archetype functions such as [brm_archetype_successive_cells()].
#' @section Contrasts:
#'   Ordinarily, ordered factors automatically use polynomial contrasts from
#'   [contr.poly()]. This is undesirable for MMRMs, so if the time variable
#'   is an ordered factor, then [brm_data()]
#'   manually sets `contrasts(data[[time]])` to a set of treatment contrasts
#'   using [contr.treatment()]. If you prefer different contrasts, please
#'   manually set `contrasts(data[[time]])` to something else after
#'   calling [brm_data()].
#' @return A data frame with the time column as an ordered factor.
#' @inheritParams brm_data
#' @param order Optional character string with the name of a variable in
#'   the data for ordering the time variable.
#'   Either `order` or `levels` must be supplied, but not both together.
#'   If `order` is supplied,
#'   the levels of `data[[order]]` must have a 1:1 correspondence with
#'   those of `data[[time]]`, and `sort(unique(data[[order]]))` must
#'   reflect the desired order of the levels of `data[[time]]`. For example,
#'   suppose you have a CDISC dataset with categorical time variable `AVISIT`
#'   and integer variable `AVISITN`. Then,
#'   `brm_data_chronologize(time = "AVISIT", order = "AVISITN")` will turn
#'   `AVISIT` into an ordered factor with levels that respect the ordering
#'   in `AVISITN`.
#' @param levels Optional character vector of levels of `data[[time]]`
#'   in chronological order. Used to turn `data[[time]]` into an
#'   ordered factor.
#'   Either `order` or `levels` must be supplied, but not both together.
#' @param time Character string with the name of the discrete time
#'   variable in the data. This is the variable that [brm_data_chronologize()]
#'   turns into an ordered factor. It needs to be specified explicitly
#'   if and only if the `data` argument was not produced by a call to
#'   [brm_data()].
#' @examples
#' data <- brm_simulate_outline(n_time = 12, n_patient = 4)
#' data$AVISIT <- gsub("_0", "_", data$time)
#' data$AVISITN <- as.integer(gsub("time_", "", data$time))
#' data[, c("AVISIT", "AVISITN")]
#' sort(unique(data$AVISIT)) # wrong order
#' data1 <- brm_data_chronologize(data, time = "AVISIT", order = "AVISITN")
#' sort(unique(data1$AVISIT)) # correct order
#' levels <- paste0("time_", seq_len(12))
#' data2 <- brm_data_chronologize(data, time = "AVISIT", levels = levels)
#' sort(unique(data2$AVISIT)) # correct order
brm_data_chronologize <- function(
  data,
  order = NULL,
  levels = NULL,
  time = attr(data, "brm_time")
) {
  if_any(
    inherits(data, "brms_mmrm_data"),
    brm_data_validate(data),
    assert(is.data.frame(data), message = "data must be a data frame")
  )
  assert_chr(time, message = "time must be a character string")
  assert_chr(
    order %|||% "x",
    message = "order must be NULL or a character string"
  )
  assert(
    time %in% colnames(data),
    message = "time must be a column name in the data"
  )
  assert(
    (order %|||% time) %in% colnames(data),
    message = "order must be NULL or a column name in the data"
  )
  assert_chr_vec(levels %|||% as.character(data[[time]][1L]))
  assert(
    (levels %|||% data[[time]][1L]) %in% data[[time]],
    message = "all elements of levels must be in data[[time]]"
  )
  assert(
    !(is.null(order) && is.null(levels)),
    message = "at least one of 'order' or 'levels' must be given"
  )
  assert(
    !(!is.null(order) && !is.null(levels)),
    message = paste(
      "'order' and 'levels' cannot both be given.",
      "Please choose one or the other."
    )
  )
  if (!is.null(order)) {
    grid <- dplyr::distinct(data[, c(time, order)])
    grid <- grid[complete.cases(grid), ]
    assert(
      !anyDuplicated(grid[[time]]),
      !anyDuplicated(grid[[order]]),
      as.character(sort(grid[[time]])) ==
        as.character(sort(unique(data[[time]]))),
      as.character(sort(grid[[order]])) ==
        as.character(sort(unique(data[[order]]))),
      message = paste(
        "Cannot create an ordered factor for the discrete time variable",
        "because the elements of the discrete time variable do not have a",
        "1:1 correspondence with the elements of the ordering variable.",
        "Please make sure variables",
        time,
        "and",
        order,
        "have a 1:1 correspondence between their levels",
        "and that",
        order,
        "has enough non-missing values to chronologize all of",
        time
      )
    )
    grid <- grid[order(grid[[order]]),, drop = FALSE] # nolint
    levels <- grid[[time]]
  }
  data[[time]] <- ordered(data[[time]], levels = levels)
  contrasts(data[[time]]) <- stats::contr.treatment(n = length(levels))
  data
}
