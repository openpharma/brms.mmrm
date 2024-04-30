brm_has_subgroup <- function(data, formula) {
  UseMethod("brm_has_subgroup")
}

#' @export
brm_has_subgroup.brms_mmrm_data <- function(data, formula) {
  attributes <- c(
    "brm_baseline_subgroup",
    "brm_baseline_subgroup_time",
    "brm_group_subgroup",
    "brm_group_subgroup_time",
    "brm_subgroup",
    "brm_subgroup_time"
  )
  any(unlist(lapply(attributes, attr, x = formula)))
}

#' @export
brm_has_subgroup.brms_mmrm_scenario <- function(data, formula) {
  !is.null(attr(data, "brm_subgroup"))
}

brm_has_nuisance <- function(data, formula) {
  UseMethod("brm_has_nuisance")
}

#' @export
brm_has_nuisance.brms_mmrm_data <- function(data, formula) {
  attributes <- c(
    "brm_baseline",
    "brm_baseline_subgroup",
    "brm_baseline_subgroup_time",
    "brm_baseline_time",
    "brm_covariates"
  )
  any(unlist(lapply(attributes, attr, x = formula)))
}

#' @export
brm_has_nuisance.brms_mmrm_scenario <- function(data, formula) {
  length(attr(data, "brm_scenario_nuisance")) > 0L
}

unname_df <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- unname(x[[i]])
  }
  x
}

zero_pad_integers <- function(x) {
  sprintf(paste0("%0", max(nchar(as.character(x))), "d"), as.integer(x))
}
