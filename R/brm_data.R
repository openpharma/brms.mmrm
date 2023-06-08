#' @title Create an MMRM dataset.
#' @export
#' @family data
#' @description Create a dataset to analyze with an MMRM.
#' @return A classed tibble with attributes which denote features of
#'   the data such as the treatment group and discrete time variables.
#' @param data Data frame or tibble with longitudinal data.
#' @param outcome Character of length 1, name of the outcome variable.
#' @param role Character of length 1. Either `"response"` if `outcome`
#'   is the raw response variable (e.g. AVAL) or `"change"` if `outcome`
#'   is change from baseline (e.g. CHG).
#' @param group Character of length 1, name of the treatment group variable.
#' @param base Character of length 1, name of the baseline response variable.
#'   Supply `NULL` to ignore or omit.
#' @param time Character of length 1, name of the discrete time variable.
#' @param patient Character of length 1, name of the patient ID variable.
#' @param covariates Character vector of names of other covariates.
#' @examples
#' set.seed(0)
#' sim <- brm_simulate()
#' data <- tibble::as_tibble(sim$data)
#' colnames(data) <- paste0("col_", colnames(data))
#' data
#' brm_data(
#'   data = data,
#'   outcome = "col_response",
#'   role = "response",
#'   group = "col_group",
#'   time = "col_time",
#'   patient = "col_patient"
#' )
brm_data <- function(
  data,
  outcome = "CHG",
  role = "change",
  base = NULL,
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0)
) {
  assert(is.data.frame(data), message = "data arg must be a data frame.")
  out <- brm_data_new(
    data = data,
    outcome = as.character(outcome),
    role = as.character(role),
    base = base,
    group = as.character(group),
    time = as.character(time),
    patient = as.character(patient),
    covariates = as.character(covariates)
  )
  brm_data_validate(data = out)
  out <- brm_data_fill(out)
  out <- brm_data_select(data = out)
  out
}

brm_data_new <- function(
  data,
  outcome,
  role,
  base,
  group,
  time,
  patient,
  covariates
) {
  out <- tibble::new_tibble(x = data, class = "brm_data")
  structure(
    out,
    outcome = outcome,
    role = role,
    base = base,
    group = group,
    time = time,
    patient = patient,
    covariates = covariates
  )
}

brm_data_validate <- function(data) {
  outcome <- attr(data, "outcome")
  role <- attr(data, "role")
  base <- attr(data, "base")
  group <- attr(data, "group")
  time <- attr(data, "time")
  patient <- attr(data, "patient")
  covariates <- attr(data, "covariates")
  assert(is.data.frame(data), message = "data must be a data frame")
  assert(inherits(data, "brm_data"), message = "data not from brm_data()")
  assert_chr(outcome, "outcome of data must be a nonempty character string")
  assert_chr(role, "role of data must be a nonempty character string")
  assert_chr(base %|||% "base", "base of data must NULL or character")
  assert_chr(group, "group of data must be a nonempty character string")
  assert_chr(time, "time of data must be a nonempty character string")
  assert_chr(patient, "patient of data must be a nonempty character string")
  assert_chr_vec(covariates, "covariates of data must be a character vector")
  assert_col(outcome, data)
  assert(
    role %in% c("response", "change"),
    message = "role must be either \"response\" or \"change\""
  )
  assert_col(base, data)
  assert_col(group, data)
  assert_col(time, data)
  assert_col(patient, data)
  assert_col(covariates, data)
  assert(
    is.numeric(data[[outcome]]),
    message = "outcome variable in the data must be numeric."
  )
  for (column in c(base, group, time, patient, covariates)) {
    assert(
      !anyNA(data[[column]]),
      message = sprintf(
        "no missing values allowed in column \"%s\"",
        column
      )
    )
  }
  for (column in c(group, time)) {
    assert(
      !is.numeric(data[[column]]),
      message = sprintf(
        paste(
          "%s column in the data must not be numeric.",
          "Should be character or factor."
        ),
        column
      )
    )
  }
}

brm_data_select <- function(data) {
  columns <- c(
    attr(data, "outcome"),
    attr(data, "base"),
    attr(data, "group"),
    attr(data, "time"),
    attr(data, "patient"),
    attr(data, "covariates")
  )
  columns <- as.character(columns)
  data[, columns, drop = FALSE]
}

brm_data_fill <- function(data) {
  outcome <- attr(data, "outcome")
  role <- attr(data, "role")
  base <- attr(data, "base")
  group <- attr(data, "group")
  time <- attr(data, "time")
  patient <- attr(data, "patient")
  covariates <- attr(data, "covariates")
  args <- list(data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = tidyr::complete, args = args)
  args <- list(.data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = dplyr::arrange, args = args)
  for (column in c(base, group, covariates)) {
    data[[column]] <- brm_data_fill_column(data[[column]], data[[patient]])
  }
  args <- list(
    .data = data,
    as.symbol(group),
    as.symbol(patient),
    as.symbol(time)
  )
  data <- do.call(what = dplyr::arrange, args = args)
  brm_data_new(
    data = data,
    outcome = outcome,
    role = role,
    base = base,
    group = group,
    time = time,
    patient = patient,
    covariates = covariates
  )
}

brm_data_fill_column <- function(x, index) {
  out <- tapply(
    X = x,
    INDEX = index,
    FUN = brm_data_locf
  )
  unlist(out, use.names = FALSE)
}

brm_data_locf <- function(x) {
  x <- zoo::na.locf(x, fromLast = FALSE, na.rm = FALSE)
  x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
  x
}
