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
  has_baseline <- !is.null(attr(data, "brm_baseline"))
  out <- nuisance_columns(
    data = data,
    prefix = prefix,
    covariates = covariates,
    baseline = baseline,
    baseline_subgroup = baseline_subgroup,
    baseline_subgroup_time = baseline_subgroup_time,
    baseline_time = baseline_time
  )
  if (!is.null(out) && ncol(out)) {
    out <- nuisance_full_rank(
      data = data,
      interest = interest,
      nuisance = out
    )
  }
  if (has_baseline) {
    out <- nuisance_mask(
      data = data,
      nuisance = out,
      prefix = prefix,
      covariates = covariates,
      baseline = baseline,
      baseline_subgroup = baseline_subgroup,
      baseline_subgroup_time = baseline_subgroup_time,
      baseline_time = baseline_time
    )
  }
  nuisance_center(out)
}

nuisance_columns <- function(
  data,
  prefix,
  covariates,
  baseline,
  baseline_subgroup,
  baseline_subgroup_time,
  baseline_time
) {
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
  }
  out
}

# Identify the rows that each baseline-derived nuisance column applies to.
# Rebuilding the columns with baseline set to 1 turns each baseline-derived
# column into its own indicator and leaves the other columns unchanged, so
# differences between the two versions reveal which columns involve baseline.
# nuisance_columns() generates the same column names in the same order given
# the same arguments, which is what lets the rebuilt columns line up with the
# ones that survived nuisance_full_rank().
nuisance_mask <- function(data, nuisance, prefix, ...) {
  if (is.null(nuisance) || !ncol(nuisance)) {
    return(nuisance)
  }
  constant <- data
  constant[[attr(data, "brm_baseline")]] <- 1
  indicator <- nuisance_columns(data = constant, prefix = prefix, ...)
  assert(
    all(colnames(nuisance) %in% colnames(indicator)),
    message = paste0(
      "could not identify the rows of the nuisance columns that involve ",
      "baseline. Please submit a bug report to ",
      "https://github.com/openpharma/brms.mmrm/issues ",
      "and include a small runnable reproducible example."
    )
  )
  indicator <- indicator[, colnames(nuisance), drop = FALSE]
  for (name in colnames(nuisance)) {
    difference <- abs(nuisance[[name]] - indicator[[name]])
    if (any(difference > .Machine$double.eps)) {
      mask <- abs(indicator[[name]]) > .Machine$double.eps
      attr(nuisance[[name]], "brm_mask") <- mask
    }
  }
  nuisance
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
    mask <- nuisance_mask_rows(data[[name]])
    mean <- mean(data[[name]][mask])
    attr(data[[name]], "brm_center") <- mean
    data[[name]][mask] <- data[[name]][mask] - mean
  }
  data
}

# Rows a nuisance column is centered over. Columns formed by interacting
# baseline with time and/or subgroup are structurally zero outside the cell
# that defines them, so they are centered only within that cell. All other
# columns are centered over the whole dataset.
nuisance_mask_rows <- function(x) {
  attr(x, "brm_mask") %|||% rep(TRUE, length(x))
}

# Message the user once per session if a nuisance column of an informative
# prior archetype was recentered by means other than brm_recenter_nuisance().
nuisance_warn_recentered <- function(data) {
  names <- nuisance_recentered_manually(data)
  if (!length(names)) {
    return(invisible())
  }
  brm_message_session(
    "In brm_transform_marginal(), these nuisance columns of the informative ",
    "prior archetype appear to have been recentered outside ",
    "brm_recenter_nuisance(): ",
    paste(names, collapse = ", "),
    ". Recentering a nuisance variable shifts the reference level of the ",
    "model, and brm_transform_marginal() assumes nuisance columns are ",
    "centered as the archetype recorded them. Please recenter with ",
    "brm_recenter_nuisance(), which keeps the recorded center in sync with ",
    "the column and respects the cell that a baseline interaction applies ",
    "to. Whenever a nuisance variable is recentered manually, it is your ",
    "responsibility to interpret the reference level of the model and the ",
    "resulting marginal means appropriately. For more information, please ",
    "visit https://openpharma.github.io/brms.mmrm/articles/archetypes.html",
    id = "nuisance_recentered"
  )
  invisible()
}

# Nuisance columns whose contents no longer agree with the center recorded in
# their "brm_center" attribute. brm_recenter_nuisance() keeps the attribute in
# sync with the column, so a disagreement means the column was recentered some
# other way and the reference level of the model has silently shifted.
# Only informative prior archetypes have nuisance columns to check.
nuisance_recentered_manually <- function(data) {
  if (!inherits(data, "brms_mmrm_archetype")) {
    return(character(0L))
  }
  nuisance <- attr(data, "brm_archetype_nuisance")
  if (!length(nuisance)) {
    return(character(0L))
  }
  defaults <- nuisance_default_centers(data)
  out <- character(0L)
  for (name in nuisance) {
    original <- nuisance_match_default(name, defaults)
    center <- attr(data[[name]], "brm_center")
    if (is.na(original) || !is.numeric(center) || length(center) != 1L) {
      next
    }
    observed <- mean(data[[name]][nuisance_mask_rows(data[[name]])])
    expected <- original - center
    tolerance <- sqrt(.Machine$double.eps) * max(1, abs(expected))
    if (abs(observed - expected) > tolerance) {
      out <- c(out, name)
    }
  }
  out
}

# The center each nuisance column would have if the archetype were rebuilt from
# the original data columns it still carries. Generating every kind of nuisance
# column gives a superset of the ones the archetype actually has, so the
# archetype does not need to record which baseline arguments built it.
nuisance_default_centers <- function(data) {
  data <- brm_data_remove_archetype(data)
  has_baseline <- !is.null(attr(data, "brm_baseline"))
  has_subgroup <- !is.null(attr(data, "brm_subgroup"))
  args <- list(
    data = data,
    prefix = "",
    covariates = TRUE,
    baseline = TRUE,
    baseline_subgroup = has_subgroup,
    baseline_subgroup_time = has_subgroup,
    baseline_time = TRUE
  )
  out <- do.call(what = nuisance_columns, args = args)
  if (is.null(out) || !ncol(out)) {
    return(numeric(0L))
  }
  if (has_baseline) {
    args$nuisance <- out
    out <- do.call(what = nuisance_mask, args = args)
  }
  centers <- lapply(out, function(x) mean(x[nuisance_mask_rows(x)]))
  unlist(centers)
}

# Match a nuisance column of an archetype to the uncentered column it came
# from. Archetype column names are the generated names with prefix_nuisance
# prepended, and the longest match wins so that e.g. a baseline-by-subgroup
# column does not match a baseline-by-subgroup-by-time column.
nuisance_match_default <- function(name, defaults) {
  candidates <- names(defaults)
  candidates <- candidates[endsWith(name, candidates)]
  if (!length(candidates)) {
    return(NA_real_)
  }
  defaults[[candidates[which.max(nchar(candidates))]]]
}
