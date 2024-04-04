brm_sep <- function() {
  out <- Sys.getenv("BRM_SEP", unset = "|")
  assert(nzchar(out), message = "BRM_SEP must be nonempty")
  out
}

names_component <- function(names, component) {
  assert_chr(component)
  assert(component %in% c("group", "subgroup", "time"))
  names <- setdiff(names, names_mcmc)
  out <- strsplit(names, split = brm_sep(), fixed = TRUE)
  if (component == "group") {
    index <- 1L
  } else if (component == "subgroup") {
    assert(isTRUE(names_have_subgroup(names)))
    index <- 2L
  } else if (component == "time") {
    index <- if_any(names_have_subgroup(names), 3L, 2L)
  }
  as.character(lapply(out, function(x) x[[index]]))
}

name_marginal <- function(group, time) {
  paste(group, time, sep = brm_sep())
}

name_marginal_subgroup <- function(group, subgroup, time) {
  paste(group, subgroup, time, sep = brm_sep())
}

names_have_subgroup <- function(names) {
  names <- setdiff(names, names_mcmc)
  matches <- lapply(
    names, function(name) {
      length(gregexpr(pattern = brm_sep(), text = name, fixed = TRUE)[[1L]])
    }
  )
  matches <- unique(as.integer(matches))
  assert(
    length(matches) == 1L,
    message = paste(
      "Found a mix of subgroup and non-subgroup names in these columns",
      "of a posterior draws data frame:",
      paste(names, collapse = ", ")
    )
  )
  matches > 1L
}

names_mcmc <- c(".chain", ".draw", ".iteration")
