brm_archetype_init <- function(
  data,
  interest,
  nuisance,
  mapping,
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
  groups <- attr(data, "brm_levels_group")
  subgroups <- attr(data, "brm_levels_subgroup")
  times <- attr(data, "brm_levels_time")
  n_group <- length(groups)
  n_subgroup <- length(subgroups)
  n_time <- length(times)
  if (brm_data_has_subgroup(data)) {
    assert(
      mapping$group == rep(groups, each = n_subgroup * n_time),
      message = "malformed or misordered mapping group levels"
    )
    assert(
      mapping$subgroup ==
        rep(rep(subgroups, times = n_group), each = n_time),
      message = "malformed or misordered mapping group levels"
    )
    assert(
      mapping$time == rep(times, times = n_group * n_subgroup),
      message = "malformed or misordered mapping group levels"
    )
  } else {
    assert(
      mapping$group == rep(groups, each = n_time),
      message = "malformed or misordered mapping group levels"
    )
    assert(
      mapping$time == rep(times, times = n_group),
      message = "malformed or misordered mapping group levels"
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

#' @export
summary.brms_mmrm_archetype <- function(object, ...) {
  formula <- brm_formula(object)
  transform <- brm_transform_marginal(object, formula, prefix = "")
  transform <- transform[, attr(object, "brm_archetype_interest")]
  marginals <- gsub(brm_sep(), ":", rownames(transform), fixed = TRUE)
  lines <- c(
    "This is the \"%s\" informative prior archetype in brms.mmrm.",
    "The following equations show the relationships between the",
    "marginal means (left-hand side) and fixed effect parameters",
    "(right-hand side). You can create informative priors for the",
    "fixed effect parameters using historical borrowing,",
    "expert elicitation, or other methods.",
    ""
  )
  name <- gsub("^brms_mmrm_", "", class(object)[1L])
  name <- gsub("_", " ", name)
  lines <- sprintf(lines, name)
  for (index in seq_along(marginals)) {
    coef <- transform[index, ]
    terms <- colnames(transform)[coef != 0]
    coef <- unname(round(coef[coef != 0], digits = 2))
    sign <- ifelse(coef < 0, "- ", "+ ")
    sign[1L] <- ""
    coef[seq_along(coef) > 1L] <- abs(coef[seq_along(coef) > 1L])
    prefix <- ifelse(coef == 1, "", paste0(coef, "*"))
    terms <- paste0(sign, prefix, terms)
    line <- paste("  ", marginals[index], "=", paste(terms, collapse = " "))
    lines <- c(lines, line)
  }
  lines <- paste("#", lines, sep = " ")
  cat(lines, sep = "\n")
  invisible()
}
