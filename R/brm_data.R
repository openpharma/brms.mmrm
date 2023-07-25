#' @title Create and preprocess an MMRM dataset.
#' @export
#' @family data
#' @description Create a dataset to analyze with an MMRM.
#' @section Preprocessing:
#'   The preprocessing steps in `brm_data()` are as follows:
#'   * Perform basic assertions to make sure the data and other arguments
#'     are properly formatted.
#'   * Convert the group and time columns to character vectors.
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
#'   Must point to a character vector in the data. Factors are converted
#'   to characters.
#' @param base Character of length 1, name of the baseline response variable.
#'   Supply `NULL` to ignore or omit.
#' @param time Character of length 1, name of the discrete time variable.
#'   Must point to a character vector in the data. Factors are converted
#'   to characters.
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
    brm_outcome = as.character(outcome),
    brm_role = as.character(role),
    brm_base = base,
    brm_group = as.character(group),
    brm_time = as.character(time),
    brm_patient = as.character(patient),
    brm_covariates = as.character(covariates)
  )
  brm_data_validate(data = out)
  brm_data_preprocess(out)
}

brm_data_new <- function(
  data,
  brm_outcome,
  brm_role,
  brm_base = NULL,
  brm_group,
  brm_time,
  brm_patient,
  brm_covariates,
  brm_levels_group = NULL,
  brm_levels_time = NULL,
  brm_labels_group = NULL,
  brm_labels_time = NULL
) {
  out <- tibble::new_tibble(x = data, class = "brm_data")
  structure(
    out,
    brm_outcome = brm_outcome,
    brm_role = brm_role,
    brm_base = brm_base,
    brm_group = brm_group,
    brm_time = brm_time,
    brm_patient = brm_patient,
    brm_covariates = brm_covariates,
    brm_levels_group = brm_levels_group,
    brm_levels_time = brm_levels_time,
    brm_labels_group = brm_labels_group,
    brm_labels_time = brm_labels_time
  )
}

brm_data_preprocess <- function(out) {
  out <- brm_data_level(out)
  out <- brm_data_fill(out)
  out <- brm_data_select(out)
  out
}

brm_data_validate <- function(data) {
  outcome <- attr(data, "brm_outcome")
  role <- attr(data, "brm_role")
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  levels_group <- attr(data, "brm_levels_group")
  levels_time <- attr(data, "brm_levels_time")
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
  assert(
    all(levels_group %in% data[[group]]),
    message = "all group levels must be in data[[group]]"
  )
  assert(
    all(levels_time %in% data[[time]]),
    message = "all time levels must be in data[[time]]"
  )
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
        "column in the data must be a character vector."
      )
    )
  }
}

brm_data_select <- function(data) {
  columns <- c(
    attr(data, "brm_outcome"),
    attr(data, "brm_base"),
    attr(data, "brm_group"),
    attr(data, "brm_time"),
    attr(data, "brm_patient"),
    attr(data, "brm_covariates")
  )
  columns <- as.character(columns)
  data[, columns, drop = FALSE]
}

brm_data_level <- function(data) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  names_group <- brm_names(data[[group]])
  names_time <- brm_names(data[[time]])
  all_group <- tibble::tibble(label = data[[group]], level = names_group)
  all_time <- tibble::tibble(label = data[[time]], level = names_time)
  data[[group]] <- as.character(names_group)
  data[[time]] <- as.character(names_time)
  meta_group <- dplyr::arrange(dplyr::distinct(all_group), level)
  meta_time <- dplyr::arrange(dplyr::distinct(all_time), level)
  attr(data, "brm_levels_group") <- as.character(meta_group$level)
  attr(data, "brm_levels_time") <- as.character(meta_time$level)
  attr(data, "brm_labels_group") <- as.character(meta_group$label)
  attr(data, "brm_labels_time") <- as.character(meta_time$label)
  data
}

brm_names <- function(x) {
  make.names(as.character(x), unique = FALSE, allow_ = TRUE)
}

brm_data_fill <- function(data) {
  attributes <- brm_data_attributes(data)
  base <- attr(data, "brm_base")
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
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
  attributes$data <- do.call(what = dplyr::arrange, args = args)
  do.call(what = brm_data_new, args = attributes)
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

brm_data_attributes <- function(data) {
  out <- attributes(data)
  out <- out[grep("^brm_", names(out), value = TRUE)]
  out
}
