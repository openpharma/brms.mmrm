brm_scenario_init <- function(
  data,
  interest,
  nuisance,
  parameterization,
  subclass
) {
  interest_data <- intersect(colnames(interest), colnames(data))
  nuisance_data <- intersect(colnames(nuisance), colnames(data))
  scenario_data <- c(interest_data, nuisance_data)
  assert(
    length(scenario_data) == 0L,
    message = paste0(
      "Existing columns in the data conflict with new columns ",
      "for the scenario: ",
      paste(scenario_data, collapse = ", "),
      ". Please rename columns and/or levels in your data as appropriate ",
      "to avoid these name conflicts."
    )
  )
  scenario <- brm_scenario_new(
    scenario = dplyr::bind_cols(interest, nuisance, data),
    data = data,
    subclass = subclass,
    brm_scenario_interest = colnames(interest),
    brm_scenario_nuisance = colnames(nuisance),
    brm_scenario_parameterization = parameterization
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
  brm_scenario_parameterization
) {
  args <- brm_data_attributes(data)
  args$.Data <- tibble::new_tibble(
    x = scenario,
    class = c(subclass, "brms_mmrm_scenario", "brms_mmrm_data")
  )
  args$brm_scenario_interest <- brm_scenario_interest
  args$brm_scenario_nuisance <- brm_scenario_nuisance
  args$brm_scenario_parameterization <- brm_scenario_parameterization
  do.call(what = structure, args = args)
}

#' @export
brm_data_validate.brms_mmrm_scenario <- function(scenario) {
  interest <- attr(scenario, "brm_scenario_interest")
  nuisance <- attr(scenario, "brm_scenario_nuisance")
  parameterization <- attr(scenario, "brm_scenario_parameterization")
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
      sort(attr(scenario, "brm_scenario_interest")),
    message = paste(
      "the \"variable\" column of the brm_scenario_parameterization",
      "attribute must agree with the values in the",
      "brm_scenario_interest attribute."
    )
  )
  assert(
    sort(unique(parameterization$group)) ==
      sort(unique(attr(data, "brm_levels_group"))),
    message = paste(
      "the \"group\" column of the brm_scenario_parameterization",
      "attribute must agree with the group levels in the",
      "brm_levels_group attribute."
    )
  )
  assert(
    sort(unique(.subset2(parameterization, "subgroup"))) ==
      sort(unique(attr(data, "brm_levels_subgroup"))),
    message = paste(
      "the \"subgroup\" column of the brm_scenario_parameterization",
      "attribute must agree with the subgroup levels in the",
      "brm_levels_subgroup attribute."
    )
  )
  assert(
    sort(unique(parameterization$time)) ==
      sort(unique(attr(data, "brm_levels_time"))),
    message = paste(
      "the \"time\" column of the brm_scenario_parameterization",
      "attribute must agree with the group levels in the",
      "brm_levels_time attribute."
    )
  )
  NextMethod()
}

scenario_nuisance <- function(data) {
  names <- c(attr(data, "brm_covariates"), attr(data, "brm_baseline"))
  names_continuous <- Filter(\(x) is.numeric(data[[x]]), names)
  names_categorical <- setdiff(names, names_continuous)
  continuous <- data[, names_continuous]
  categorical <- model.matrix(~0 + ., data[, names_categorical])
  out <- dplyr::bind_cols(continuous, categorical)
  for (name in colnames(out)) {
    out[[name]] <- out[[name]] - mean(out[[name]])
  }
  colnames(out) <- brm_levels(paste0("nuisance_", colnames(out)))
  out
}
