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
#' @param baseline Character of length 1,
#'   name of the baseline response variable.
#'   Only relevant if the response variable is change from baseline.
#'   Supply `NULL` to ignore or omit.
#' @param group Character of length 1, name of the treatment group variable.
#'   For most analyses, we recommend that the `group` variable is
#'   a character vector or unordered factor.
#' @param subgroup Character of length 1, optional name of the a
#'   discrete subgroup variable. Set to `NULL` to omit the subgroup (default).
#'   For most subgroup, we recommend that the `subgroup` variable is
#'   a character vector or unordered factor.
#' @param time Character of length 1, name of the discrete time variable.
#'   For most analyses, please use an ordered factor for the `time` column
#'   in the data. This ensures the time points sort in chronological order,
#'   which ensures the correctness of informative prior archetypes and
#'   autoregressive / moving average correlation structures.
#'
#'   Ordinarily, ordered factors automatically use polynomial contrasts from
#'   [contr.poly()]. This is undesirable for MMRMs, so if the time variable
#'   is an ordered factor, then [brm_data()]
#'   manually sets `contrasts(data[[time]])` to a set of treatment contrasts
#'   using [contr.treatment()]. If you prefer different contrasts, please
#'   manually set `contrasts(data[[time]])` to something else after
#'   calling [brm_data()].
#' @param patient Character of length 1, name of the patient ID variable.
#' @param covariates Character vector of names of other covariates.
#' @param missing Character of length 1, name of an optional variable
#'   in a simulated dataset to indicate which outcome values should be missing.
#'   Set to `NULL` to omit.
#' @param reference_group Atomic value of length 1, Level of the `group`
#'   column to indicate the control group.
#'   `reference_group` only applies to the post-processing that happens
#'   in functions like [brm_marginal_draws()] downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param reference_subgroup Atomic value of length 1,
#'   level of the `subgroup` column
#'   to use as a reference for pairwise differences in when computing
#'   marginal means downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param reference_time Atomic value of length 1 or `NULL`,
#'   level of the `time` column to indicate the baseline time point.
#'   This value must be `NULL` if the outcome
#'   variable is already change from baseline. If the outcome
#'   is raw response, then `reference_time` may or may not be `NULL`.
#'   [brm_marginal_draws()] and downstream functions calculate posterior
#'   inference on change from baseline if and only if
#'   `reference_time` is not `NULL`.
#'
#'   Note: `reference_time` only applies to the post-processing that happens
#'   in functions like [brm_marginal_draws()] downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param level_baseline Deprecated on 2024-01-11 (version 0.2.0.9002).
#'   Use `reference_time` instead.
#' @param level_control Deprecated on 2024-01-11 (version 0.2.0.9002).
#'   Use `reference_group` instead.
#' @examples
#' set.seed(0)
#' data <- brm_simulate_simple()$data
#' colnames(data) <- paste0("col_", colnames(data))
#' data
#' brm_data(
#'   data = data,
#'   outcome = "col_response",
#'   role = "response",
#'   group = "col_group",
#'   time = "col_time",
#'   patient = "col_patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
brm_data <- function(
  data,
  outcome = "CHG",
  role = "change",
  baseline = NULL,
  group = "TRT01P",
  subgroup = NULL,
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0L),
  missing = NULL,
  reference_group = "Placebo",
  reference_subgroup = NULL,
  reference_time = NULL,
  level_baseline = NULL,
  level_control = NULL
) {
  assert(is.data.frame(data), message = "data arg must be a data frame.")
  if (!is.null(level_control)) {
    brm_deprecate(
      "level_control was deprecated on 2024-01-11 (version 0.2.0.9002). ",
      "Use reference_group instead. Setting reference_group <- level_control",
      " for this one call to brm_data()."
    )
    reference_group <- level_control
  }
  if (!is.null(level_baseline)) {
    brm_deprecate(
      "level_baseline was deprecated on 2024-01-11 (version 0.2.0.9002). ",
      "Use reference_time instead. Setting reference_time <- level_baseline",
      " for this one call to brm_data()."
    )
    reference_time <- level_baseline
  }
  out <- brm_data_new(
    data = data,
    brm_outcome = as.character(outcome),
    brm_role = as.character(role),
    brm_baseline = baseline,
    brm_group = as.character(group),
    brm_subgroup = subgroup,
    brm_time = as.character(time),
    brm_patient = as.character(patient),
    brm_covariates = as.character(covariates),
    brm_missing = missing,
    brm_reference_group = reference_group,
    brm_reference_subgroup = reference_subgroup,
    brm_reference_time = reference_time
  )
  brm_data_validate(data = out)
  brm_data_preprocess(out)
}

brm_data_new <- function(
  data,
  brm_outcome = NULL,
  brm_role = NULL,
  brm_baseline = NULL,
  brm_group = NULL,
  brm_subgroup = NULL,
  brm_time = NULL,
  brm_patient = NULL,
  brm_covariates = NULL,
  brm_missing = NULL,
  brm_reference_group = NULL,
  brm_reference_subgroup = NULL,
  brm_reference_time = NULL
) {
  out <- tibble::new_tibble(x = data, class = "brms_mmrm_data")
  structure(
    out,
    brm_outcome = brm_outcome,
    brm_role = brm_role,
    brm_baseline = brm_baseline,
    brm_group = brm_group,
    brm_subgroup = brm_subgroup,
    brm_time = brm_time,
    brm_patient = brm_patient,
    brm_covariates = brm_covariates,
    brm_missing = brm_missing,
    brm_reference_group = brm_reference_group,
    brm_reference_subgroup = brm_reference_subgroup,
    brm_reference_time = brm_reference_time
  )
}

brm_data_preprocess <- function(out) {
  out <- brm_data_fill(out)
  out <- brm_data_select(out)
  out <- brm_time_contrasts(out)
  out
}

brm_data_validate <- function(data) {
  UseMethod("brm_data_validate")
}

#' @export
brm_data_validate.default <- function(data) {
  outcome <- attr(data, "brm_outcome")
  role <- attr(data, "brm_role")
  baseline <- attr(data, "brm_baseline")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  missing <- attr(data, "brm_missing")
  reference_group <- attr(data, "brm_reference_group")
  reference_subgroup <- attr(data, "brm_reference_subgroup")
  reference_time <- attr(data, "brm_reference_time")
  assert(is.data.frame(data), message = "data must be a data frame")
  assert(
    inherits(data, "brms_mmrm_data"),
    message = "please use brm_data() to preprocess your data"
  )
  assert_chr(outcome, "outcome of data must be a nonempty character string")
  assert_chr(role, "role of data must be a nonempty character string")
  assert_chr(
    baseline %|||% "x",
    "baseline must NULL or a nonempty character string"
  )
  assert_chr(group, "group of data must be a nonempty character string")
  assert_chr(
    subgroup %|||% "x",
    "subgroup of data must be NULL or a nonempty character string"
  )
  assert_chr(time, "time of data must be a nonempty character string")
  assert_chr(patient, "patient of data must be a nonempty character string")
  assert_chr_vec(covariates, "covariates of data must be a character vector")
  assert_chr(missing %|||% "missing", "missing must NULL or character")
  assert_chr(reference_group, "reference_group must be a nonempty string")
  assert_chr(
    reference_subgroup %|||% "x",
    paste(
      "reference_subgroup must NULL or a nonempty element of the subgroup",
      "column in the data"
    )
  )
  assert_chr(
    reference_time %|||% "x",
    paste(
      "reference_time must NULL or a nonempty element of the time column",
      "in the data"
    )
  )
  assert(
    role %in% c("response", "change"),
    message = "role must be either \"response\" or \"change\""
  )
  assert_col(outcome, data)
  assert_col(baseline, data)
  assert_col(group, data)
  assert_col(subgroup, data)
  assert_col(time, data)
  assert_col(patient, data)
  assert_col(covariates, data)
  assert_col(missing, data)
  assert_machine_names(outcome)
  assert_machine_names(baseline %|||% "baseline")
  assert_machine_names(group)
  assert_machine_names(subgroup)
  assert_machine_names(time)
  assert_machine_names(patient)
  assert_machine_names(covariates)
  assert_machine_names(missing)
  assert(
    reference_group,
    !anyNA(.),
    length(.) == 1L,
    . %in% data[[group]],
    message = "reference_group must be an element of data[[group]]"
  )
  if (!is.null(subgroup)) {
    assert(
      reference_subgroup,
      !anyNA(.),
      length(.) == 1L,
      . %in% data[[subgroup]],
      message = paste(
        "reference_subgroup must be an element of data[[subgroup]]"
      )
    )
  }
  if (!is.null(reference_time)) {
    assert(
      reference_time,
      !anyNA(.),
      length(.) == 1L,
      . %in% data[[time]],
      message = "reference_time must be an element of data[[time]]"
    )
  }
  sep <- brm_sep()
  elements <- c(
    group,
    subgroup,
    time,
    data[[group]],
    data[[time]]
  )
  if (!is.null(subgroup)) {
    elements <- c(elements, data[[subgroup]])
  }
  assert(
    !any(grepl(pattern = sep, x = unique(elements), fixed = TRUE)),
    message = sprintf(
      paste(
        "The separation string \"%s\" must not be contained in",
        "the names or elements of the group, subgroup,",
        "or time columns in the data.",
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
  if (!is.null(baseline)) {
    assert(
      is.numeric(data[[outcome]]),
      message = "baseline variable must be numeric if supplied."
    )
  }
  for (column in c(baseline, group, subgroup, time, patient, covariates)) {
    assert(
      !anyNA(data[[column]]),
      message = sprintf(
        "no missing values allowed in column \"%s\"",
        column
      )
    )
  }
  for (column in c(group, subgroup, time)) {
    assert(
      is.atomic(data[[column]]) || is.factor(data[[column]]),
      message = paste(
        column,
        "column in the data must be an atomic or factor type."
      )
    )
  }
  if (role == "change") {
    assert(
      is.null(reference_time),
      message = "reference_time must be NULL if role is \"change\"."
    )
  }
}

brm_data_select <- function(data) {
  columns <- c(
    attr(data, "brm_outcome"),
    attr(data, "brm_missing"),
    attr(data, "brm_baseline"),
    attr(data, "brm_group"),
    attr(data, "brm_subgroup"),
    attr(data, "brm_time"),
    attr(data, "brm_patient"),
    attr(data, "brm_covariates")
  )
  data[, as.character(unique(columns)), drop = FALSE]
}

brm_data_fill <- function(data) {
  UseMethod("brm_data_fill")
}

#' @export
brm_data_fill.default <- function(data) {
  brm_error(
    "brm_data_fill() is only valid for brm_data() objects,",
    "not arbitrary data or informative prior archetypes."
  )
}

#' @export
brm_data_fill.brms_mmrm_data <- function(data) {
  class <- class(data)
  attributes <- brm_data_attributes(data)
  baseline <- attr(data, "brm_baseline")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  missing <- attr(data, "brm_missing")
  interest <- attr(data, "brm_archetype_interest")
  nuisance <- attr(data, "brm_archetype_nuisance")
  args <- list(data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = tidyr::complete, args = args)
  args <- list(.data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = dplyr::arrange, args = args)
  columns <- c(
    baseline,
    group,
    subgroup,
    covariates,
    missing,
    interest,
    nuisance
  )
  for (column in columns) {
    data[[column]] <- brm_data_fill_column(data[[column]], data[[patient]])
  }
  args <- if_any(
    is.null(subgroup),
    list(
      .data = data,
      as.symbol(group),
      as.symbol(patient),
      as.symbol(time)
    ),
    list(
      .data = data,
      as.symbol(group),
      as.symbol(subgroup),
      as.symbol(patient),
      as.symbol(time)
    )
  )
  out <- do.call(what = dplyr::arrange, args = args)
  class(out) <- class
  for (name in names(attributes)) {
    attr(out, name) <- attributes[[name]]
  }
  times <- brm_levels(out[[time]])
  assert(
    out[[time]] == rep(times, times = nrow(data) / length(times)),
    message = paste(
      "data could not be filled. Please submit a bug report to",
      "https://github.com/openpharma/brms.mmrm/issues",
      "and include a reproducible example."
    )
  )
  out
}

brm_time_contrasts <- function(data) {
  time <- attr(data, "brm_time")
  if (is.ordered(data[[time]])) {
    n <- length(unique(data[[time]]))
    contrasts(data[[time]]) <- stats::contr.treatment(n = n)
  }
  data
}

brm_levels <- function(x) {
  if_any(is.factor(x), levels(x), sort(unique(x)))
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

brm_data_has_subgroup <- function(data) {
  !is.null(attr(data, "brm_subgroup"))
}
