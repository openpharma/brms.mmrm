archetype_nuisance <- function(
  data,
  interest,
  prefix,
  covariates,
  baseline,
  baseline_subgroup,
  baseline_subgroup_time,
  baseline_time
) {
  assert(
    !all(is.na(data[[attr(data, "brm_outcome")]])),
    message = paste(
      "all variables in the outcome variable are missing,",
      "which prevents brms.mmrm from checking that the model matrix",
      "will have full rank when supplied to brms.",
      "Please supply a dataset with at least some non-missing outcomes."
    )
  )
  text <- "'%s' in must be TRUE or FALSE."
  assert_lgl(covariates, sprintf(text, "covariates"))
  assert_lgl(baseline, sprintf(text, "baseline"))
  assert_lgl(baseline_subgroup, sprintf(text, "baseline_subgroup"))
  assert_lgl(baseline_subgroup_time, sprintf(text, "baseline_subgroup_time"))
  assert_lgl(baseline_time, sprintf(text, "baseline_time"))
  out <- tibble::new_tibble(list(), nrow = nrow(data))
  has_baseline <- !is.null(attr(data, "brm_baseline"))
  has_subgroup <- !is.null(attr(data, "brm_subgroup"))
  if (covariates) {
    out <- dplyr::bind_cols(out, nuisance_covariates(data))
  }
  if (baseline && has_baseline) {
    out <- dplyr::bind_cols(out, nuisance_baseline(data))
  }
  if (baseline_subgroup && has_baseline && has_subgroup) {
    out <- dplyr::bind_cols(out, nuisance_baseline_subgroup(data))
  }
  if (baseline_subgroup_time && has_baseline && has_subgroup) {
    out <- dplyr::bind_cols(out, nuisance_baseline_subgroup_time(data))
  }
  if (baseline_time && has_baseline) {
    out <- dplyr::bind_cols(out, nuisance_baseline_time(data))
  }
  if (!is.null(out) && ncol(out)) {
    colnames(out) <- paste0(prefix, colnames(out))
    colnames(out) <- make.names(colnames(out), unique = FALSE, allow_ = TRUE)
    out <- nuisance_full_rank(
      data = data,
      interest = interest,
      nuisance = out
    )
  }
  nuisance_center(out)
}

nuisance_covariates <- function(data) {
  names <- attr(data, "brm_covariates")
  names_continuous <- Filter(\(x) is.numeric(data[[x]]), names)
  names_categorical <- setdiff(names, names_continuous)
  out <- data[, names_continuous]
  categorical <- data[, names_categorical]
  if (ncol(categorical)) {
    colnames(categorical) <- paste0(colnames(categorical), "_")
    out <- dplyr::bind_cols(out, model.matrix(~ 0 + ., categorical))
  }
  out
}

nuisance_baseline <- function(data) {
  data[, attr(data, "brm_baseline"), drop = FALSE]
}

nuisance_baseline_subgroup <- function(data) {
  baseline <- attr(data, "brm_baseline")
  subgroup <- attr(data, "brm_subgroup")
  formula <- as.formula(paste0("~ 0 + ", baseline, ":", subgroup))
  nuisance_baseline_terms(data, baseline, formula)
}

nuisance_baseline_subgroup_time <- function(data) {
  baseline <- attr(data, "brm_baseline")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  formula <- as.formula(paste0("~ 0 + ", baseline, ":", subgroup, ":", time))
  nuisance_baseline_terms(data, baseline, formula)
}

nuisance_baseline_time <- function(data) {
  baseline <- attr(data, "brm_baseline")
  time <- attr(data, "brm_time")
  formula <- as.formula(paste0("~ 0 + ", baseline, ":", time))
  nuisance_baseline_terms(data, baseline, formula)
}

nuisance_baseline_terms <- function(data, baseline, formula) {
  matrix <- model.matrix(object = formula, data = data)
  tibble::as_tibble(as.data.frame(matrix))
}

nuisance_full_rank <- function(data, interest, nuisance) {
  out <- nuisance
  index <- !is.na(data[[attr(data, "brm_outcome")]])
  interest <- interest[index, ]
  nuisance <- nuisance[index, ]
  matrix <- as.matrix(dplyr::bind_cols(interest, nuisance))
  names <- columns_full_rank(matrix)
  dropped_interest <- setdiff(colnames(interest), names)
  assert(
    length(dropped_interest) < 1L,
    message = paste0(
      "dropped columns of interest while trying to make the ",
      "model matrix full-rank: ",
      paste(dropped_interest, collapse = ", "),
      ". Please submit a bug report to ",
      "https://github.com/openpharma/brms.mmrm/issues ",
      "and include a small runnable reproducible example."
    )
  )
  out[, intersect(colnames(out), names), drop = FALSE]
}

columns_full_rank <- function(x) {
  x <- drop_zero_columns(x)
  columns <- colnames(x)
  qr <- base::qr(x)
  rank <- qr$rank
  columns[qr$pivot[seq_len(rank)]]
}

drop_zero_columns <- function(x) {
  sums <- colSums(abs(x))
  x[, sums > .Machine$double.eps, drop = FALSE]
}

nuisance_center <- function(data) {
  for (name in colnames(data)) {
    mean <- mean(data[[name]])
    attr(data[[name]], "brm_center") <- mean
    data[[name]] <- data[[name]] - mean
  }
  data
}
