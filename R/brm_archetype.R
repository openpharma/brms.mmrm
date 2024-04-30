brm_archetype_init <- function(
  data,
  interest,
  nuisance,
  parameterization,
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
  archetype <- brm_archetype_new(
    archetype = dplyr::bind_cols(interest, nuisance, data),
    data = data,
    subclass = subclass,
    brm_archetype_interest = colnames(interest),
    brm_archetype_nuisance = colnames(nuisance),
    brm_archetype_parameterization = parameterization
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
  brm_archetype_parameterization
) {
  args <- brm_data_attributes(data)
  args$.Data <- tibble::new_tibble(
    x = archetype,
    class = c(subclass, "brms_mmrm_archetype", "brms_mmrm_data")
  )
  args$brm_archetype_interest <- brm_archetype_interest
  args$brm_archetype_nuisance <- brm_archetype_nuisance
  args$brm_archetype_parameterization <- brm_archetype_parameterization
  do.call(what = structure, args = args)
}

#' @export
brm_data_validate.brms_mmrm_archetype <- function(data) {
  interest <- attr(data, "brm_archetype_interest")
  nuisance <- attr(data, "brm_archetype_nuisance")
  parameterization <- attr(data, "brm_archetype_parameterization")
  assert_chr_vec(
    interest,
    "brm_archetype_interest attribute must be a character vector"
  )
  assert_chr_vec(
    nuisance,
    "brm_archetype_nuisance attribute must be a character vector"
  )
  assert(
    is.data.frame(parameterization),
    message = "brm_archetype_parameterization attribute must be a data frame"
  )
  assert(
    c("group", "time", "variable") %in% colnames(parameterization),
    message = paste(
      "brm_archetype_parameterization attribute must have columns",
      "\"group\", \"time\", and \"variable\"."
    )
  )
  assert(
    colnames(parameterization) %in%
      c("group", "subgroup", "time", "variable"),
    message = paste(
      "brm_archetype_parameterization attribute columns cannot be",
      "anything other than",
      "\"group\", \"subgroup\", \"time\", or \"variable\"."
    )
  )
  assert(
    sort(parameterization$variable) ==
      sort(attr(data, "brm_archetype_interest")),
    message = paste(
      "the \"variable\" column of the brm_archetype_parameterization",
      "attribute must agree with the values in the",
      "brm_archetype_interest attribute."
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
