brm_scenario_init <- function(
  data,
  interest,
  nuisance,
  parameterization,
  prefix_interest,
  prefix_nuisance,
  subclass
) {
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
  baseline <- if_any(
    is.null(attr(data, "brm_baseline")),
    NULL,
    paste0(prefix_nuisance, attr(data, "brm_baseline"))
  )
  scenario <- brm_scenario_new(
    scenario = dplyr::bind_cols(interest, nuisance, data),
    data = data,
    subclass = subclass,
    brm_scenario_interest = colnames(interest),
    brm_scenario_nuisance = colnames(nuisance),
    brm_scenario_parameterization = parameterization,
    brm_scenario_baseline = baseline,
    brm_scenario_prefix_interest = prefix_interest,
    brm_scenario_prefix_nuisance = prefix_nuisance
  )
  brm_data_validate(scenario)
  scenario
}

brm_scenario_new <- function(
  scenario,
  data,
  subclass,
  brm_scenario_interest,
  brm_scenario_nuisance,
  brm_scenario_parameterization,
  brm_scenario_baseline,
  brm_scenario_prefix_interest,
  brm_scenario_prefix_nuisance
) {
  args <- brm_data_attributes(data)
  args$.Data <- tibble::new_tibble(
    x = scenario,
    class = c(subclass, "brms_mmrm_scenario", "brms_mmrm_data")
  )
  args$brm_scenario_interest <- brm_scenario_interest
  args$brm_scenario_nuisance <- brm_scenario_nuisance
  args$brm_scenario_parameterization <- brm_scenario_parameterization
  args$brm_scenario_baseline <- brm_scenario_baseline
  args$brm_scenario_prefix_interest <- brm_scenario_prefix_interest
  args$brm_scenario_prefix_nuisance <- brm_scenario_prefix_nuisance
  do.call(what = structure, args = args)
}

#' @export
brm_data_validate.brms_mmrm_scenario <- function(data) {
  assert_chr(
    attr(data, "brm_scenario_prefix_interest") %||nzchar% "x",
    "brm_scenario_prefix_interest must be a single character string"
  )
  assert_chr(
    attr(data, "brm_scenario_prefix_nuisance") %||nzchar% "x",
    "brm_scenario_prefix_nuisance must be a single character string"
  )
  baseline <- attr(data, "brm_baseline")
  scenario_baseline <- attr(data, "brm_scenario_baseline")
  if (!is.null(baseline)) {
    assert(
      scenario_baseline ==
        paste0(attr(data, "brm_scenario_prefix_nuisance"), baseline),
      message = "brm_baseline name must match brm_scenario_baseline name"
    )
    assert(
      data[[scenario_baseline]] == data[[baseline]] - mean(data[[baseline]]),
      message = paste(
        "brm_scenario_baseline must be the centered version of brm_baseline"
      )
    )
  }
  interest <- attr(data, "brm_scenario_interest")
  nuisance <- attr(data, "brm_scenario_nuisance")
  parameterization <- attr(data, "brm_scenario_parameterization")
  assert_chr_vec(
    interest,
    "brm_scenario_interest attribute must be a character vector"
  )
  assert_chr_vec(
    nuisance,
    "brm_scenario_nuisance attribute must be a character vector"
  )
  assert(
    is.data.frame(parameterization),
    message = "brm_scenario_parameterization attribute must be a data frame"
  )
  assert(
    c("group", "time", "variable") %in% colnames(parameterization),
    message = paste(
      "brm_scenario_parameterization attribute must have columns",
      "\"group\", \"time\", and \"variable\"."
    )
  )
  assert(
    colnames(parameterization) %in%
      c("group", "subgroup", "time", "variable"),
    message = paste(
      "brm_scenario_parameterization attribute columns cannot be",
      "anything other than",
      "\"group\", \"subgroup\", \"time\", or \"variable\"."
    )
  )
  assert(
    sort(parameterization$variable) ==
      sort(attr(data, "brm_scenario_interest")),
    message = paste(
      "the \"variable\" column of the brm_scenario_parameterization",
      "attribute must agree with the values in the",
      "brm_scenario_interest attribute."
    )
  )
  assert(
    !anyDuplicated(parameterization$variable),
    message = "parameterization$variable must have all unique values"
  )
  groups <- attr(data, "brm_levels_group")
  subgroups <- attr(data, "brm_levels_subgroup")
  times <- attr(data, "brm_levels_time")
  n_group <- length(groups)
  n_subgroup <- length(subgroups)
  n_time <- length(times)
  if (brm_data_has_subgroup(data)) {
    assert(
      parameterization$group == rep(groups, each = n_subgroup * n_time),
      message = "malformed or misordered parameterization group levels"
    )
    assert(
      parameterization$subgroup ==
        rep(rep(subgroups, times = n_group), each = n_time),
      message = "malformed or misordered parameterization group levels"
    )
    assert(
      parameterization$time == rep(times, times = n_group * n_subgroup),
      message = "malformed or misordered parameterization group levels"
    )
  } else {
    assert(
      parameterization$group == rep(groups, each = n_time),
      message = "malformed or misordered parameterization group levels"
    )
    assert(
      parameterization$time == rep(times, times = n_group),
      message = "malformed or misordered parameterization group levels"
    )
  }
  NextMethod()
}

scenario_nuisance <- function(data, prefix) {
  names <- c(attr(data, "brm_covariates"), attr(data, "brm_baseline"))
  names_continuous <- Filter(\(x) is.numeric(data[[x]]), names)
  names_categorical <- setdiff(names, names_continuous)
  out <- data[, names_continuous]
  categorical <- data[, names_categorical]
  if (ncol(categorical)) {
    colnames(categorical) <- paste0(colnames(categorical), "_")
    out <- dplyr::bind_cols(out, model.matrix(~ 0 + ., categorical))
  }
  for (name in colnames(out)) {
    out[[name]] <- out[[name]] - mean(out[[name]])
  }
  if (ncol(out)) {
    colnames(out) <- brm_levels(paste0(prefix, colnames(out)))
  }
  out
}

brm_data_remove_scenario <- function(data) {
  attributes <- brm_scenario_attributes(data)
  data <- data[, setdiff(colnames(data), attr(data, "brm_scenario_interest"))]
  data <- data[, setdiff(colnames(data), attr(data, "brm_scenario_nuisance"))]
  for (name in names(attributes)) {
    attr(data, name) <- NULL
  }
  class(data) <- class(brm_data_new(list(x = "x")))
  data
}

brm_scenario_attributes <- function(data) {
  out <- attributes(data)
  out <- out[grep("^brm_scenario_", names(out), value = TRUE)]
  out
}
