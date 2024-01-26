brm_sep <- function() {
  Sys.getenv("BRM_SEP", unset = "|")
}

gsub_group <- function(names) {
  out <- strsplit(names, split = brm_sep(), fixed = TRUE)
  as.character(lapply(out, function(x) x[[1L]]))
}

gsub_time <- function(names) {
  out <- strsplit(names, split = brm_sep(), fixed = TRUE)
  as.character(lapply(out, function(x) x[[2L]]))
}

name_marginal <- function(group, time) {
  sprintf("%s%s%s", group, brm_sep(), time)
}

name_marginal_subgroup <- function(group, subgroup, time) {
  sprintf("%s%s%s%s%s", group, brm_sep(), subgroup, brm_sep(), time)
}

names_group <- function(draws) {
  gsub_group(setdiff(colnames(draws), names_mcmc))
}

names_time <- function(draws) {
  gsub_time(setdiff(colnames(draws), names_mcmc))
}

names_mcmc <- c(".chain", ".draw", ".iteration")

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
