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
#'   is the discrete time variable in the data. This declares
#'   one standard deviation parameter for each time point in the data.
#' @return A base R formula to parameterize `sigma`, the linear-scale
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
#' @param baseline_subgroup Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup interaction, `FALSE` to omit.
#' @param baseline_subgroup_time Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup-by-time interaction,
#'   `FALSE` to omit.
#' @param baseline_time Logical of length 1.
#'   `TRUE` to include baseline-by-time interaction, `FALSE` to omit.
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
#' brm_formula(data)
#' brm_formula(data = data, intercept = FALSE, baseline = FALSE)
#' formula <- brm_formula(
#'   data = data,
#'   intercept = FALSE,
#'   baseline = FALSE,
#'   group = FALSE
#' )
#' formula
#' # Optional: set the contrast option, which determines the model matrix.
#' options(contrasts = c(unordered = "contr.SAS", ordered = "contr.poly"))
#' # See the fixed effect mapping you get from the data:
#' head(brms::make_standata(formula = formula, data = data)$X)
#' # Specify a different contrast method to use an alternative
#' # mapping when fitting the model with brm_model():
#' options(
#'   contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")
#' )
#' # different model matrix than before:
#' head(brms::make_standata(formula = formula, data = data)$X)
#' # Formula on an informative prior archetype:
#' data <- brm_simulate_outline(
#'   n_group = 2,
#'   n_patient = 100,
#'   n_time = 4,
#'   rate_dropout = 0,
#'   rate_lapse = 0
#' ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n())) |>
#'   brm_data_change() |>
#'   brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
#'   brm_simulate_categorical(
#'     names = "biomarker3",
#'     levels = c("present", "absent")
#'   )
#' archetype <- brm_archetype_successive_cells(data)
#' formula <- brm_formula(data = archetype)
#' formula
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
    unlist(lapply(name_covariates, term, condition = covariates)),
    term_correlation(
      correlation = correlation,
      name_time = name_time,
      name_patient = name_patient,
      autoregressive_order = autoregressive_order,
      moving_average_order = moving_average_order,
      residual_covariance_arma_estimation
    )
  )
  terms <- terms[nzchar(terms)]
  right <- paste(terms, collapse = " + ")
  formula_full <- stats::as.formula(paste("sigma ~", right))
  formula_check <- stats::as.formula(paste("~", right))
  if (check_rank) {
    formula_sigma_check_rank(data = data, formula = formula_check)
  }
  formula_full
}

formula_check_rank <- function(data, formula) {
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
