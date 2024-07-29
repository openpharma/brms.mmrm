brm_archetype_init <- function(
  data,
  interest,
  mapping,
  intercept,
  baseline,
  baseline_subgroup,
  baseline_subgroup_time,
  baseline_time,
  covariates,
  prefix_nuisance,
  subclass
) {
  assert_lgl(intercept, "intercept must be TRUE or FALSE")
  if (intercept) {
    interest[[1L]] <- 1L
  }
  nuisance <- archetype_nuisance(
    data = data,
    interest = interest,
    prefix = prefix_nuisance,
    covariates = covariates,
    baseline = baseline,
    baseline_subgroup = baseline_subgroup,
    baseline_subgroup_time = baseline_subgroup_time,
    baseline_time = baseline_time
  )
  data_interest <- intersect(colnames(data), colnames(interest))
  data_nuisance <- intersect(colnames(data), colnames(nuisance))
  interest_nuisance <- intersect(colnames(interest), colnames(nuisance))
  assert(
    !length(data_interest),
    message = paste0(
      "conflicting column names between the data and generated fixed ",
      "effects of interest: ",
      paste(data_interest, collapse = ", "),
      ". Please choose a prefix_interest value to make column names unique."
    )
  )
  assert(
    !length(data_nuisance),
    message = paste0(
      "conflicting column names between the data and generated nuisance ",
      "fixed effects of interest: ",
      paste(data_nuisance, collapse = ", "),
      ". Please choose a prefix_nuisance value to make column names unique."
    )
  )
  assert(
    !length(interest_nuisance),
    message = paste0(
      "conflicting column names between generated fixed effects",
      "of interest and generated nuisance variables: ",
      paste(interest_nuisance, collapse = ", "),
      ". Please choose different values for prefix_interest and ",
      "prefix_nuisance value to make column names unique."
    )
  )
  archetype <- brm_archetype_new(
    archetype = dplyr::bind_cols(interest, nuisance, data),
    data = data,
    subclass = subclass,
    brm_archetype_interest = colnames(interest),
    brm_archetype_nuisance = colnames(nuisance),
    brm_archetype_mapping = mapping
  )
  brm_data_validate(archetype)
  archetype
}

brm_archetype_new <- function(
  archetype,
  data,
  subclass,
  brm_archetype_interest,
  brm_archetype_nuisance,
  brm_archetype_mapping
) {
  args <- brm_data_attributes(data)
  args$.Data <- tibble::new_tibble(
    x = archetype,
    class = c(subclass, "brms_mmrm_archetype", "brms_mmrm_data")
  )
  args$brm_archetype_interest <- brm_archetype_interest
  args$brm_archetype_nuisance <- brm_archetype_nuisance
  args$brm_archetype_mapping <- brm_archetype_mapping
  do.call(what = structure, args = args)
}

#' @export
brm_data_validate.brms_mmrm_archetype <- function(data) {
  interest <- attr(data, "brm_archetype_interest")
  nuisance <- attr(data, "brm_archetype_nuisance")
  mapping <- attr(data, "brm_archetype_mapping")
  assert_chr_vec(
    interest,
    "brm_archetype_interest attribute must be a character vector"
  )
  assert_chr_vec(
    nuisance,
    "brm_archetype_nuisance attribute must be a character vector"
  )
  assert(
    is.data.frame(mapping),
    message = "brm_archetype_mapping attribute must be a data frame"
  )
  assert(
    c("group", "time", "variable") %in% colnames(mapping),
    message = paste(
      "brm_archetype_mapping attribute must have columns",
      "\"group\", \"time\", and \"variable\"."
    )
  )
  assert(
    colnames(mapping) %in%
      c("group", "subgroup", "time", "variable"),
    message = paste(
      "brm_archetype_mapping attribute columns cannot be",
      "anything other than",
      "\"group\", \"subgroup\", \"time\", or \"variable\"."
    )
  )
  assert(
    sort(mapping$variable) ==
      sort(attr(data, "brm_archetype_interest")),
    message = paste(
      "the \"variable\" column of the brm_archetype_mapping",
      "attribute must agree with the values in the",
      "brm_archetype_interest attribute."
    )
  )
  assert(
    !anyDuplicated(mapping$variable),
    message = "mapping$variable must have all unique values"
  )
  groups <- brm_levels(data[[attr(data, "brm_group")]])
  subgroups <- if_any(
    is.null(attr(data, "brm_subgroup")),
    character(0L),
    brm_levels(data[[attr(data, "brm_subgroup")]])
  )
  times <-  brm_levels(data[[attr(data, "brm_time")]])
  n_group <- length(groups)
  n_subgroup <- length(subgroups)
  n_time <- length(times)
  if (brm_data_has_subgroup(data)) {
    assert(
      all(mapping$group %in% groups),
      message = "informative prior archetype mapping has bad group levels"
    )
    assert(
      all(mapping$subgroup %in% subgroups),
      message = "informative prior archetype mapping has bad subgroup levels"
    )
    assert(
      all(mapping$time %in% times),
      message = "informative prior archetype mapping has bad time levels"
    )
  } else {
    assert(
      all(mapping$group %in% groups),
      message = "informative prior archetype mapping has bad group levels"
    )
    assert(
      all(mapping$time %in% times),
      message = "informative prior archetype mapping has bad time levels"
    )
  }
  NextMethod()
}

brm_data_remove_archetype <- function(data) {
  attributes <- brm_archetype_attributes(data)
  data <- data[, setdiff(colnames(data), attr(data, "brm_archetype_interest"))]
  data <- data[, setdiff(colnames(data), attr(data, "brm_archetype_nuisance"))]
  for (name in names(attributes)) {
    attr(data, name) <- NULL
  }
  class(data) <- class(brm_data_new(list(x = "x")))
  data
}

brm_archetype_attributes <- function(data) {
  out <- attributes(data)
  out <- out[grep("^brm_archetype_", names(out), value = TRUE)]
  out
}

#' @title Summarize an informative prior archetype.
#' @export
#' @keywords internal
#' @description For an informative prior archetype, show
#'   the transformation from model parameters to marginal means.
#' @return Return a character vector with linear equations
#'   that map model parameters to marginal means. If the `message`
#'   argument is `TRUE` (default) then this character vector is returned
#'   invisibly and a verbose description of the equations is printed.
#' @param object The informative prior archetype to summarize.
#' @param message TRUE to print an informative message about the archetype
#'   and invisibly return a character vector of equations. `FALSE`
#'   to forgo verbose messages and non-invisibly return the equations.
#' @param ... Not used, but required for S3 methods that inherit from
#'   the base generic [summary()].
#' @examples
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
#' dplyr::select(
#'   data,
#'   group,
#'   time,
#'   patient,
#'   starts_with("biomarker"),
#'   starts_with("status")
#' )
#' archetype <- brm_archetype_successive_cells(data)
#' equations <- summary(archetype)
#' print(equations)
#' summary(archetype, message = FALSE)
summary.brms_mmrm_archetype <- function(object, message = TRUE, ...) {
  formula <- brm_formula(object)
  transform <- brm_transform_marginal(
    object,
    formula,
    average_within_subgroup = FALSE,
    prefix = ""
  )
  lines <- c(
    "This is the \"%s\" informative prior archetype in brms.mmrm.",
    "The following equations show the relationships between the",
    "marginal means (left-hand side) and fixed effect parameters",
    "(right-hand side).",
    ""
  )
  name <- gsub("^brms_mmrm_", "", class(object)[1L])
  name <- gsub("_", " ", name)
  lines <- sprintf(lines, name)
  transform <- transform[, attr(object, "brm_archetype_interest")]
  out <- brm_transform_marginal_lines(transform)
  lines_transform <- paste(" ", out)
  lines <- paste("#", c(lines, lines_transform), sep = " ")
  if (message) {
    message(paste(lines, collapse = "\n"))
    return(invisible(out))
  } else {
    out
  }
}

brm_archetype_assert_prefixes <- function(prefix_interest, prefix_nuisance) {
  assert_chr(
    prefix_interest %||nzchar% "x",
    "prefix_interest must be a single character string"
  )
  assert_chr(
    prefix_nuisance %||nzchar% "x",
    "prefix_nuisance must be a single character string"
  )
  assert(
    prefix_interest != prefix_nuisance,
    message = "prefix_interest and prefix_nuisance must be different"
  )
}
