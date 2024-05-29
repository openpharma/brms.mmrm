#' @title Formula for standard deviation parameters
#' @export
#' @family models
#' @description Parameterize standard deviations using
#'   a formula for the `sigma` argument of [brm_formula()].
#' @details In `brms`, the standard deviations of the residuals are
#'   modeled through a parameter vector called `sigma`. `brms.mmrm`
#'   always treats `sigma` as a distributional parameter
#'   (<https://paul-buerkner.github.io/brms/articles/brms_distreg.html>).
#'   [brm_formula_sigma()] lets you control the parameterization of `sigma`.
#'   The output of [brm_formula_sigma()] serves as input to the `sigma`
#'   argument of [brm_formula()].
#'
#'   The default `sigma` formula is `sigma ~ 0 + time`, where `time`
#'   is the discrete time variable in the data. This is the usual
#'   heterogeneous variance structure which declares
#'   one standard deviation parameter for each time point in the data.
#'   Alternatively, you could write
#'   `brm_formula_sigma(data, intercept = TRUE, time = FALSE)`.
#'   This will produce `sigma ~ 1`, which yields a single scalar variance
#'   (a structure termed "homogeneous variance").
#'
#'   With arguments like `baseline` and `covariates`, you can
#'   specify extremely complicated variance structures. However,
#'   if baseline or covariates are used, then the output of
#'   [brm_marginal_draws()] omit effect size due to the statistical
#'   challenges of calculating marginal means of draws of `sigma`
#'   for this uncommon scenario.
#' @return A base R formula with S3 class `"brms_mmrm_formula_sigma"`.
#'   This formula controls the parameterization of `sigma`, the linear-scale
#'   `brms` distributional parameters which represent standard deviations.
#' @inheritParams brm_formula
#' @param check_rank `TRUE` to check the rank of the model matrix
#'   for `sigma` and throw an error if rank deficiency is detected.
#'   `FALSE` to skip this check.
#'   Rank-deficiency may cause `sigma` to be non-identifiable,
#'   may prevent the MCMC from converging.
#' @param baseline Logical of length 1.
#'   `TRUE` to include an additive effect for baseline
#'   response, `FALSE` to omit.
#'   If `TRUE`, then effect size will be omitted from the output of
#'   [brm_marginal_draws()].
#' @param baseline_subgroup Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup interaction, `FALSE` to omit.
#'   If `TRUE`, then effect size will be omitted from the output of
#'   [brm_marginal_draws()].
#' @param baseline_subgroup_time Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup-by-time interaction,
#'   `FALSE` to omit.
#'   If `TRUE`, then effect size will be omitted from the output of
#'   [brm_marginal_draws()].
#' @param baseline_time Logical of length 1.
#'   `TRUE` to include baseline-by-time interaction, `FALSE` to omit.
#'   If `TRUE`, then effect size will be omitted from the output of
#'   [brm_marginal_draws()].
#' @param group_subgroup Logical of length 1.
#'   `TRUE` to include group-by-subgroup interaction, `FALSE` to omit.
#' @param group_subgroup_time Logical of length 1.
#'   `TRUE` to include group-by-subgroup-by-time interaction, `FALSE` to omit.
#' @param group_time Logical of length 1.
#' @param subgroup Logical of length 1.
#'   `TRUE` to include additive fixed effects for subgroup levels,
#'   `FALSE` to omit.
#' @param subgroup_time Logical of length 1.
#'   `TRUE` to include subgroup-by-time interaction, `FALSE` to omit.
#' @param time Logical of length 1.
#' @param covariates Logical of length 1.
#'   `TRUE` (default) to include any additive covariates declared with
#'   the `covariates` argument of [brm_data()],
#'   `FALSE` to omit.
#'   If `TRUE`, then effect size will be omitted from the output of
#'   [brm_marginal_draws()].
#' @param baseline_subgroup Logical of length 1.
#' @examples
#' set.seed(0)
#' data <- brm_data(
#'   data = brm_simulate_simple()$data,
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
#' homogeneous <- brm_formula_sigma(data, time = FALSE, intercept = TRUE)
#' by_group <- brm_formula_sigma(data, group = TRUE, intercept = TRUE)
#' homogeneous
#' by_group
#' brm_formula(data, sigma = homogeneous)
#' brm_formula(data, sigma = by_group)
brm_formula_sigma <- function(
  data,
  check_rank = TRUE,
  intercept = FALSE,
  baseline = FALSE,
  baseline_subgroup = FALSE,
  baseline_subgroup_time = FALSE,
  baseline_time = FALSE,
  covariates = FALSE,
  group = FALSE,
  group_subgroup = FALSE,
  group_subgroup_time = FALSE,
  group_time = FALSE,
  subgroup = FALSE,
  subgroup_time = FALSE,
  time = TRUE
) {
  brm_data_validate(data)
  text <- "'%s' in brm_formula_sigma() must be TRUE or FALSE."
  assert_lgl(check_rank, sprintf(text, "check_rank"))
  assert_lgl(intercept, sprintf(text, "intercept"))
  assert_lgl(baseline, sprintf(text, "baseline"))
  assert_lgl(baseline_subgroup, sprintf(text, "baseline_subgroup"))
  assert_lgl(baseline_subgroup_time, sprintf(text, "baseline_subgroup_time"))
  assert_lgl(baseline_time, sprintf(text, "baseline_time"))
  assert_lgl(group, sprintf(text, "group"))
  assert_lgl(group_subgroup, sprintf(text, "group_subgroup"))
  assert_lgl(group_subgroup_time, sprintf(text, "group_subgroup_time"))
  assert_lgl(group_time, sprintf(text, "group_"))
  assert_lgl(subgroup, sprintf(text, "subgroup"))
  assert_lgl(subgroup_time, sprintf(text, "subgroup_time"))
  assert_lgl(time, sprintf(text, "time"))
  assert_lgl(covariates, sprintf(text, "covariates"))
  expect_baseline <- baseline ||
    baseline_subgroup ||
    baseline_subgroup_time ||
    baseline_time
  if (expect_baseline) {
    assert_chr(
      attr(data, "brm_baseline"),
      message = "brm_data() found no baseline column in the data."
    )
  }
  expect_subgroup <-  baseline_subgroup ||
    baseline_subgroup_time ||
    group_subgroup ||
    group_subgroup_time ||
    subgroup ||
    subgroup_time
  if (expect_subgroup) {
    assert_chr(
      attr(data, "brm_subgroup"),
      message = "brm_data() found no subgroup column in the data."
    )
  }
  exclude_effect_size <- baseline ||
    baseline_subgroup ||
    baseline_subgroup_time ||
    baseline_time ||
    covariates
  allow_effect_size <- !exclude_effect_size
  name_role <- attr(data, "brm_role")
  name_baseline <- attr(data, "brm_baseline")
  name_group <- attr(data, "brm_group")
  name_subgroup <- attr(data, "brm_subgroup")
  name_time <- attr(data, "brm_time")
  name_patient <- attr(data, "brm_patient")
  name_covariates <- attr(data, "brm_covariates")
  terms <- c(
    term("0", !intercept),
    term(name_baseline, baseline),
    term(c(name_baseline, name_subgroup), baseline_subgroup),
    term(c(name_baseline, name_subgroup, name_time), baseline_subgroup_time),
    term(c(name_baseline, name_time), baseline_time),
    term(name_group, group),
    term(c(name_group, name_subgroup), group_subgroup),
    term(c(name_group, name_subgroup, name_time), group_subgroup_time),
    term(c(name_group, name_time), group_time),
    term(name_subgroup, subgroup),
    term(c(name_subgroup, name_time), subgroup_time),
    term(name_time, time),
    unlist(lapply(name_covariates, term, condition = covariates))
  )
  terms <- terms[nzchar(terms)] %||% "1"
  right <- paste(terms, collapse = " + ")
  formula_full <- stats::as.formula(paste("sigma ~", right))
  formula_check <- stats::as.formula(paste("~", right))
  if (check_rank) {
    formula_sigma_check_rank(data = data, formula = formula_check)
  }
  formula_sigma_new(
    formula = formula_full,
    brm_allow_effect_size = allow_effect_size
  )
}

formula_sigma_new <- function(formula, brm_allow_effect_size) {
  structure(
    formula,
    class = unique(c("brms_mmrm_formula_sigma", class(formula))),
    brm_allow_effect_size = brm_allow_effect_size
  )
}

brm_formula_sigma_validate <- function(formula) {
  assert(
    formula,
    inherits(., "brms_mmrm_formula_sigma"),
    inherits(., "formula"),
    message = paste(
      "expected a formula produced by brm_formula_sigma(), but",
      "found a different kind of object."
    )
  )
  assert(
    attr(formula, "brm_allow_effect_size"),
    is.logical(.),
    !anyNA(.),
    length(.) == 1L,
    message = paste(
      "expected the sigma formula from brm_formula_sigma() to have",
      "a logical scalar 'brm_allow_effect_size' attribute, but",
      "found a different kind of object in the attribute."
    )
  )
}

formula_sigma_check_rank <- function(data, formula) {
  matrix <- model.matrix(object = formula, data = data)
  rank <- qr(matrix)$rank
  assert(
    ncol(matrix) == as.integer(rank),
    message = paste0(
      "model matrix of sigma has ",
      ncol(matrix),
      " columns but rank ",
      rank,
      ". Please consider a different parameterization to make the ",
      "model matrix full-rank. Otherwise, the sigma parameters may not be ",
      "identifiable and MCMC sampling may not converge. ",
      "Set check_rank = FALSE in brm_formula_sigma() to suppress this error."
    )
  )
}
