#' @title Create and preprocess an MMRM dataset.
#' @export
#' @family data
#' @description Create a dataset to analyze with an MMRM.
#' @section Preprocessing:
#'   The preprocessing steps in `brm_data()` are as follows:
#'   * Perform basic assertions to make sure the data and other arguments
#'     are properly formatted.
#'   * Sanitize the levels of the group and time columns using
#'     `make.names(unique = FALSE, allow_ = TRUE)` to ensure agreement
#'     between the data and the output of `brms`.
#'   * For each implicitly missing outcome observation, add explicit row
#'     with the outcome variable equal to `NA_real_`.
#'   * Arrange the rows of the data by group, then patient, then discrete time.
#'   * Select only the columns of the data relevant to an MMRM analysis.
#' @section Separation string:
#'   Post-processing in [brm_marginal_draws()] names each of the
#'   group-by-time marginal means with the delimiting character string
#'   from `Sys.getenv("BRM_SEP", unset = "|")`. Neither the column names
#'   nor element names of the group and time variables can contain
#'   this string. To set a custom string yourself, use
#'   `Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")`.
#' @return A classed tibble with attributes which denote features of
#'   the data such as the treatment group and discrete time variables.
#' @param data Data frame or tibble with longitudinal data.
#' @param outcome Character of length 1, name of the outcome variable.
#' @param role Character of length 1. Either `"response"` if `outcome`
#'   is the raw response variable (e.g. AVAL) or `"change"` if `outcome`
#'   is change from baseline (e.g. CHG).
#' @param group Character of length 1, name of the treatment group variable.
#'   Must point to a character vector or factor in the data.
#' @param base Character of length 1, name of the baseline response variable.
#'   Supply `NULL` to ignore or omit.
#' @param time Character of length 1, name of the discrete time variable.
#'   Must point to a character vector or factor in the data.
#' @param patient Character of length 1, name of the patient ID variable.
#' @param covariates Character vector of names of other covariates.
#' @examples
#' set.seed(0)
#' data <- brm_simulate()$data
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
    covariates = as.character(covariates),
    levels_group = NULL,
    levels_time = NULL
  )
  brm_data_validate(data = out)
  out <- brm_data_level(out)
  out <- brm_data_fill(out)
  out <- brm_data_select(out)
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
  covariates,
  levels_group,
  levels_time
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
    covariates = covariates,
    levels_group = levels_group,
    levels_time = levels_time
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
  assert(
    role %in% c("response", "change"),
    message = "role must be either \"response\" or \"change\""
  )
  assert_col(outcome, data)
  assert_col(base, data)
  assert_col(group, data)
  assert_col(time, data)
  assert_col(patient, data)
  assert_col(covariates, data)
  assert_machine_names(outcome)
  assert_machine_names(base %|||% "base")
  assert_machine_names(group)
  assert_machine_names(time)
  assert_machine_names(patient)
  assert_machine_names(covariates)
  sep <- brm_sep()
  elements <- c(group, time, unique(data[[group]]), unique(data[[time]]))
  assert(
    !any(grepl(pattern = sep, x = elements, fixed = TRUE)),
    message = sprintf(
      paste(
        "The separation string \"%s\" must not be contained in",
        "the names or elements of the group or time columns in the data.",
        "Either remove this string or set a different separation string",
        "with Sys.setenv(BRM_SEP = \"YOUR_SEPARATION_STRING\")."
      ),
      sep
    )
  )
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
      is.character(data[[column]]) || is.factor(data[[column]]),
      message = paste(
        column,
        "column in the data must be a character vector or factor."
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

brm_data_level <- function(data) {
  group <- attr(data, "group")
  time <- attr(data, "time")
  data[[group]] <- brm_data_sanitize_factor(data[[group]])
  data[[time]] <- brm_data_sanitize_factor(data[[time]])
  attr(data, "levels_group") <- as.character(sort(unique(data[[group]])))
  attr(data, "levels_time") <- as.character(sort(unique(data[[time]])))
  data
}

brm_data_sanitize_factor <- function(x) {
  UseMethod("brm_data_sanitize_factor")
}

brm_data_sanitize_factor.character <- function(x) {
  make.names(x, unique = FALSE, allow_ = TRUE)
}

brm_data_sanitize_factor.factor <- function(x) {
  levels(x) <- make.names(levels(x), unique = FALSE, allow_ = TRUE)
  x
}

brm_data_fill <- function(data) {
  outcome <- attr(data, "outcome")
  role <- attr(data, "role")
  base <- attr(data, "base")
  group <- attr(data, "group")
  time <- attr(data, "time")
  patient <- attr(data, "patient")
  covariates <- attr(data, "covariates")
  levels_group <- attr(data, "levels_group")
  levels_time <- attr(data, "levels_time")
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
    covariates = covariates,
    levels_group = levels_group,
    levels_time = levels_time
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

brm_sep <- function() {
  Sys.getenv("BRM_SEP", unset = "|")
}
