#' @title Model formula
#' @export
#' @family models
#' @description Build a model formula for an MMRM, either for a generic
#'   [brm_data()] dataset or an informative prior archetype.
#' @section [brm_data()] formulas:
#'   For a [brm_data()] dataset,
#'   [brm_formula()] builds an R formula for an MMRM based on
#'   the details in the data and your choice of mapping.
#'   Customize your mapping by toggling on or off
#'   the various `TRUE`/`FALSE` arguments of [brm_formula()],
#'   such as `intercept`, `baseline`, and `group_time`.
#'   All plausible additive effects, two-way interactions, and
#'   three-way interactions can be specified. The following interactions
#'   are not supported:
#'   * Any interactions with the concomitant covariates you specified in the
#'     `covariates` argument of [brm_data()].
#'   * Any interactions which include baseline response and treatment
#'     group together. Rationale: in a randomized controlled experiment,
#'     baseline and treatment group assignment should be uncorrelated.
#' @section Formulas for informative prior archetypes:
#'   Functions like [brm_archetype_successive_cells()]
#'   tailor datasets to informative prior archetypes. For these specialized
#'   tailored datasets, [brm_formula()] works differently. It still applies
#'   the variance and correlation structure of your choosing, and it still
#'   lets you choose whether to adjust for nuisance covariates,
#'   but it no longer lets you toggle on/off individual terms in the model,
#'   such as `intercept`, `baseline`, or `group`. Instead, to ensure the
#'   correct interpretation of the parameters, [brm_formula()] uses
#'   the `x_*` and `nuisance_*` columns generated by
#'   `brm_archetype_successive_cells(
#'    prefix_interest = "x_", prefix_nuisance = "nuisance_")`.
#' @section Parameterization:
#'   For a formula on a [brm_data()] dataset,
#'   the formula is not the only factor
#'   that determines the fixed effect mapping.
#'   The ordering of the categorical variables in the data,
#'   as well as the `contrast` option in R, affect the
#'   construction of the model matrix. To see the model
#'   matrix that will ultimately be used in [brm_model()],
#'   run [brms::make_standata()] and examine the `X` element
#'   of the returned list. See the examples below for a
#'   demonstration.
#' @return An object of class `"brmsformula"` returned from
#'   `brms::brmsformula()`. It contains the fixed effect mapping,
#'   correlation structure, and residual variance structure.
#' @param data A classed data frame from [brm_data()], or an informative
#'   prior archetype from a function like [brm_archetype_successive_cells()].
#' @param intercept Logical of length 1.
#'   `TRUE` (default) to include an intercept, `FALSE` to omit.
#' @param baseline Logical of length 1.
#'   `TRUE` to include an additive effect for baseline
#'   response, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a baseline
#'   variable in the dataset.
#'   Ignored for informative prior archetypes.
#'   For informative prior archetypes, this option should be set in
#'   functions like [brm_archetype_successive_cells()] rather than in
#'   [brm_formula()] in order to make sure columns are appropriately
#'   centered and the underlying model matrix has full rank.
#' @param baseline_subgroup Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup interaction, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared baseline
#'   and subgroup variables in the dataset.
#'   Ignored for informative prior archetypes.
#'   For informative prior archetypes, this option should be set in
#'   functions like [brm_archetype_successive_cells()] rather than in
#'   [brm_formula()] in order to make sure columns are appropriately
#'   centered and the underlying model matrix has full rank.
#' @param baseline_subgroup_time Logical of length 1.
#'   `TRUE` to include baseline-by-subgroup-by-time interaction,
#'   `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared baseline
#'   and subgroup variables in the dataset.
#'   Ignored for informative prior archetypes.
#'   For informative prior archetypes, this option should be set in
#'   functions like [brm_archetype_successive_cells()] rather than in
#'   [brm_formula()] in order to make sure columns are appropriately
#'   centered and the underlying model matrix has full rank.
#' @param baseline_time Logical of length 1.
#'   `TRUE` to include baseline-by-time interaction, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a baseline
#'   variable in the dataset.
#'   Ignored for informative prior archetypes.
#'   For informative prior archetypes, this option should be set in
#'   functions like [brm_archetype_successive_cells()] rather than in
#'   [brm_formula()] in order to make sure columns are appropriately
#'   centered and the underlying model matrix has full rank.
#' @param group Logical of length 1.
#'   `TRUE` (default) to include additive effects for
#'   treatment groups, `FALSE` to omit.
#' @param group_subgroup Logical of length 1.
#'   `TRUE` to include group-by-subgroup interaction, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a subgroup
#'   variable in the dataset.
#' @param group_subgroup_time Logical of length 1.
#'   `TRUE` to include group-by-subgroup-by-time interaction, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a subgroup
#'   variable in the dataset.
#' @param group_time Logical of length 1.
#'   `TRUE` (default) to include group-by-time interaction, `FALSE` to omit.
#' @param subgroup Logical of length 1.
#'   `TRUE` to include additive fixed effects for subgroup levels,
#'   `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a subgroup
#'   variable in the dataset.
#' @param subgroup_time Logical of length 1.
#'   `TRUE` to include subgroup-by-time interaction, `FALSE` to omit.
#'   Default is `TRUE` if [brm_data()] previously declared a subgroup
#'   variable in the dataset.
#' @param time Logical of length 1.
#'   `TRUE` (default) to include a additive effect for discrete time,
#'   `FALSE` to omit.
#' @param covariates Logical of length 1.
#'   `TRUE` (default) to include any additive covariates declared with
#'   the `covariates` argument of [brm_data()],
#'   `FALSE` to omit.
#'   For informative prior archetypes, this option is set in
#'   functions like [brm_archetype_successive_cells()] rather than in
#'   [brm_formula()] in order to make sure columns are appropriately
#'   centered and the underlying model matrix has full rank.
#' @param baseline_subgroup Logical of length 1.
#' @param sigma A formula produced by [brm_formula_sigma()].
#'   The formula is a base R formula with S3 class
#'   `"brms_mmrm_formula_sigma"`, and it controls
#'   the parameterization of the residual standard deviations `sigma`.
#' @param correlation Character of length 1, name of the correlation
#'   structure. The correlation matrix is a square `T x T` matrix, where
#'   `T` is the number of discrete time points in the data.
#'   This matrix describes the correlations between time points in the same
#'   patient, as modeled in the residuals. Different patients are modeled
#'   as independent. The `correlation` argument controls how this matrix
#'   is parameterized, and the choices given by `brms` are listed at
#'   <https://paulbuerkner.com/brms/reference/autocor-terms.html>,
#'   and the choice is ultimately encoded in the main body of the
#'   output formula through terms like `unstru()` and `arma()`, some
#'   of which are configurable through arguments
#'   `autoregressive_order`, `moving_average_order`, and
#'   `residual_covariance_arma_estimation` of [brm_formula()].
#'   Choices in `brms.mmrm`:
#'   * `"unstructured"`: the default/recommended option, a fully parameterized
#'     covariance matrix with a unique scalar parameter for each unique pair
#'     of discrete time points. C.f.
#'     <https://paulbuerkner.com/brms/reference/unstr.html>.
#'   * `"autoregressive_moving_average"`: autoregressive moving
#'     average (ARMA), c.f.
#'     <https://paulbuerkner.com/brms/reference/arma.html>.
#'   * `"autoregressive"`: autoregressive (AR), c.f.
#'     <https://paulbuerkner.com/brms/reference/ar.html>.
#'   * `"moving_average"`: moving average (MA), c.f.
#'     <https://paulbuerkner.com/brms/reference/ma.html>.
#'   * `"compound_symmetry`: compound symmetry, c.f.
#'     <https://paulbuerkner.com/brms/reference/cosy.html>.
#'   * `"diagonal"`: declare independent time points within patients.
#' @param autoregressive_order Nonnegative integer,
#'   autoregressive order for the `"autoregressive_moving_average"`
#'   and `"autoregressive"` correlation structures.
#' @param moving_average_order Nonnegative integer,
#'   moving average order for the `"autoregressive_moving_average"`
#'   and `"moving_average"` correlation structures.
#' @param residual_covariance_arma_estimation `TRUE` or `FALSE`,
#'   whether to estimate ARMA effects using residual covariance matrices.
#'   Directly supplied to the `cov` argument in `brms` for
#'   `"autoregressive_moving_average"`, `"autoregressive"`, and
#'   `"moving_average"` correlation structures. C.f.
#'   <https://paulbuerkner.com/brms/reference/arma.html>.
#' @param model_missing_outcomes Logical of length 1, `TRUE`
#'   to impute missing outcomes during model fitting as described in the
#'   "Imputation during model fitting" section of
#'   <https://paulbuerkner.com/brms/articles/brms_missings.html>.
#'   Specifically, if the outcome variable is `y`, then the formula will
#'   begin with `y | mi() ~ ...` instead of simply `y ~ ...`.
#'   Set to `FALSE` (default) to forgo this kind of imputation
#'   and discard missing observations from the data
#'   just prior to fitting the model inside [brm_model()]. See
#'   <https://opensource.nibr.com/bamdd/src/02h_mmrm.html#what-estimand-does-mmrm-address> #nolint
#'   to understand the standard assumptions and decisions regarding MMRMs
#'   and missing outcomes.
#' @param check_rank `TRUE` to check the rank of the model matrix and
#'   throw an error if rank deficiency is detected. `FALSE` to skip
#'   this check. Rank-deficient models may have non-identifiable
#'   parameters and it is recommended to choose a full-rank mapping.
#' @param warn_ignored Set to `TRUE`
#'   to throw a warning if ignored arguments are specified,
#'   `FALSE` otherwise.
#' @param center `TRUE` to center the columns of the model matrix before
#'   fitting the model if the model formula includes an intercept
#'   term controlled by `brms`. `FALSE` to skip centering. Centering usually
#'   leads to more computationally efficient sampling in the presence
#'   of an intercept, but it changes
#'   the interpretation of the intercept parameter if included in the model
#'   (as explained in the help file of `brms::brmsformula()`).
#'   Informative prior archetypes always use `center = FALSE`
#'   and use an intercept not controlled by `brms.mmrm` to ensure the
#'   intercept parameter is interpretable and compatible with
#'   user-defined priors.
#' @param ... Named arguments to specific [brm_formula()] methods.
#' @param effect_baseline Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `baseline` instead.
#' @param effect_group Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `group` instead.
#' @param effect_time Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `time` instead.
#' @param interaction_baseline Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `baseline_time` instead.
#' @param interaction_group Deprecated on 2024-01-16 (version 0.0.2.9002).
#'   Use `group_time` instead.
#' @examples
#' set.seed(0)
#' data <- brm_data(
#'   data = brm_simulate_simple()$data,
#'   outcome = "response",
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
#' # Standard deviations of residuals are distributional parameters that can
#' # regress on variables in the data.
#' homogeneous <- brm_formula_sigma(data, time = FALSE)
#' by_group <- brm_formula_sigma(data, group = TRUE, intercept = TRUE)
#' homogeneous
#' by_group
#' brm_formula(data, sigma = homogeneous)
#' brm_formula(data, sigma = by_group)
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
brm_formula <- function(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  ...
) {
  UseMethod("brm_formula")
}

#' @rdname brm_formula
#' @export
#' @method brm_formula default
brm_formula.default <- function(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  intercept = TRUE,
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline")),
  covariates = TRUE,
  group = TRUE,
  group_subgroup = !is.null(attr(data, "brm_subgroup")),
  group_subgroup_time = !is.null(attr(data, "brm_subgroup")),
  group_time = TRUE,
  subgroup = !is.null(attr(data, "brm_subgroup")),
  subgroup_time = !is.null(attr(data, "brm_subgroup")),
  time = TRUE,
  center = TRUE,
  ...,
  effect_baseline = NULL,
  effect_group = NULL,
  effect_time = NULL,
  interaction_baseline = NULL,
  interaction_group = NULL
) {
  brm_data_validate(data)
  text <- "'%s' in brm_formula() must be TRUE or FALSE."
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
  assert_lgl(
    residual_covariance_arma_estimation,
    sprintf(text, "residual_covariance_arma_estimation")
  )
  assert_lgl(check_rank, sprintf(text, "check_rank"))
  assert(
    autoregressive_order,
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "autoregressive_order must be a nonnegative integer of length 1"
  )
  assert(
    moving_average_order,
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "moving_average_order must be a nonnegative integer of length 1"
  )
  assert_lgl(center, "center must be TRUE or FALSE")
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
  text <- paste0(
    "%s was deprecated on 2024-01-16 (version 0.0.2.9002).",
    "Use %s instead."
  )
  if (!is.null(effect_baseline)) {
    brm_deprecate(sprintf(text, "effect_baseline", "baseline"))
    baseline <- effect_baseline
  }
  if (!is.null(effect_group)) {
    brm_deprecate(sprintf(text, "effect_group", "group"))
    group <- effect_group
  }
  if (!is.null(effect_time)) {
    brm_deprecate(sprintf(text, "effect_time", "time"))
    time <- effect_time
  }
  if (!is.null(interaction_baseline)) {
    brm_deprecate(sprintf(text, "interaction_baseline", "baseline_time"))
    baseline_time <- interaction_baseline
  }
  if (!is.null(interaction_group)) {
    brm_deprecate(sprintf(text, "interaction_group", "group_time"))
    group_time <- interaction_group
  }
  brm_formula_validate_correlation(correlation)
  brm_formula_sigma_validate(sigma)
  name_outcome <- attr(data, "brm_outcome")
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
  terms <- terms[nzchar(terms)] %||% "1"
  right <- paste(terms, collapse = " + ")
  term_outcome <- if_any(
    model_missing_outcomes,
    paste(name_outcome, "| mi()"),
    name_outcome
  )
  formula_fixed <- stats::as.formula(paste(term_outcome, "~", right))
  brms_formula <- brms::brmsformula(
    formula = formula_fixed,
    sigma,
    center = center
  )
  formula <- brm_formula_new(
    formula = brms_formula,
    brm_intercept = intercept,
    brm_baseline = baseline,
    brm_baseline_subgroup = baseline_subgroup,
    brm_baseline_subgroup_time = baseline_subgroup_time,
    brm_baseline_time = baseline_time,
    brm_group = group,
    brm_group_subgroup = group_subgroup,
    brm_group_subgroup_time = group_subgroup_time,
    brm_group_time = group_time,
    brm_subgroup = subgroup,
    brm_subgroup_time = subgroup_time,
    brm_time = time,
    brm_covariates = covariates,
    brm_correlation = correlation,
    brm_autoregressive_order = autoregressive_order,
    brm_moving_average_order = moving_average_order,
    brm_residual_covariance_arma_estimation =
      residual_covariance_arma_estimation,
    brm_model_missing_outcomes = model_missing_outcomes,
    brm_allow_effect_size = attr(sigma, "brm_allow_effect_size")
  )
  brm_formula_validate(formula)
  if (check_rank) {
    formula_check_rank(data = data, formula = formula)
  }
  formula
}

#' @rdname brm_formula
#' @export
#' @method brm_formula brms_mmrm_archetype
brm_formula.brms_mmrm_archetype <- function(
  data,
  model_missing_outcomes = FALSE,
  check_rank = TRUE,
  sigma = brms.mmrm::brm_formula_sigma(data = data, check_rank = check_rank),
  correlation = "unstructured",
  autoregressive_order = 1L,
  moving_average_order = 1L,
  residual_covariance_arma_estimation = FALSE,
  ...,
  warn_ignored = TRUE
) {
  brm_data_validate(data)
  brm_formula_validate_correlation(correlation)
  brm_formula_sigma_validate(sigma)
  assert_lgl(
    residual_covariance_arma_estimation,
    "residual_covariance_arma_estimation must be TRUE or FALSE"
  )
  assert(
    autoregressive_order,
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "autoregressive_order must be a nonnegative integer of length 1"
  )
  assert(
    moving_average_order,
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "moving_average_order must be a nonnegative integer of length 1"
  )
  assert_lgl(warn_ignored, message = "warn_ignored must be TRUE or FALSE")
  args <- list(...)
  ignored <- c(
    "baseline",
    "baseline_subgroup",
    "baseline_subgroup_time",
    "baseline_time"
  )
  if (any(names(args) %in% ignored) && warn_ignored) {
    message <- paste(
      "brm_formula() ignores baseline-related arguments",
      "for informative prior archetypes",
      "(baseline, baseline_subgroup, baseline_subgroup_time",
      "and baseline_time).",
      "Please instead set these baseline arguments",
      "in the archetype function, e.g. brm_archetype_effects()",
      "Set warn_ignored = FALSE to suppress this warning."
    )
    brm_warn(message)
  }
  name_outcome <- attr(data, "brm_outcome")
  name_time <- attr(data, "brm_time")
  name_patient <- attr(data, "brm_patient")
  interest <- attr(data, "brm_archetype_interest")
  nuisance <- attr(data, "brm_archetype_nuisance")
  terms <- c(
    term("0", TRUE),
    unlist(lapply(interest, term, condition = TRUE)),
    unlist(lapply(nuisance, term, condition = TRUE)),
    term_correlation(
      correlation = correlation,
      name_time = name_time,
      name_patient = name_patient,
      autoregressive_order = autoregressive_order,
      moving_average_order = moving_average_order,
      residual_covariance_arma_estimation
    )
  )
  terms <- terms[nzchar(terms)] %||% "1"
  right <- paste(terms, collapse = " + ")
  term_outcome <- if_any(
    model_missing_outcomes,
    paste(name_outcome, "| mi()"),
    name_outcome
  )
  formula_fixed <- stats::as.formula(paste(term_outcome, "~", right))
  brms_formula <- brms::brmsformula(
    formula = formula_fixed,
    sigma,
    center = FALSE
  )
  formula <- brm_formula_archetype_new(
    formula = brms_formula,
    brm_correlation = correlation,
    brm_autoregressive_order = autoregressive_order,
    brm_moving_average_order = moving_average_order,
    brm_residual_covariance_arma_estimation =
      residual_covariance_arma_estimation,
    brm_allow_effect_size = attr(sigma, "brm_allow_effect_size"),
    brm_model_missing_outcomes = model_missing_outcomes
  )
  brm_formula_validate(formula)
  if (check_rank) {
    formula_check_rank(data = data, formula = formula)
  }
  formula
}

term <- function(labels, condition) {
  if_any(condition, paste0(labels, collapse = ":"), character(0L))
}

term_correlation <- function(
  correlation,
  name_time,
  name_patient,
  autoregressive_order,
  moving_average_order,
  residual_covariance_arma_estimation
) {
  if (identical(as.character(correlation), "diagonal")) {
    return(NULL)
  }
  fun <- switch(
    correlation,
    unstructured = "unstr",
    autoregressive_moving_average = "arma",
    autoregressive = "ar",
    moving_average = "ma",
    compound_symmetry = "cosy"
  )
  args <- list(
    time = as.symbol(name_time),
    gr = as.symbol(name_patient),
    p = autoregressive_order,
    q = moving_average_order,
    cov = residual_covariance_arma_estimation
  )[names(formals(getNamespace("brms")[[fun]]))]
  call <- as.call(c(as.symbol(fun), args))
  paste(deparse(call), collapse = " ")
}

formula_check_rank <- function(data, formula) {
  missing <- is.na(data[[attr(data, "brm_outcome")]])
  if (all(missing)) {
    data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  } else {
    data <- data[!is.na(data[[attr(data, "brm_outcome")]]), ]
  }
  matrix <- brms::make_standata(data = data, formula = formula)$X
  rank <- qr(matrix)$rank
  assert(
    ncol(matrix) == as.integer(rank),
    message = paste0(
      "model matrix has ",
      ncol(matrix),
      " columns but rank ",
      rank,
      " (with missing outcomes removed if the outcome column is valid). ",
      "Please consider a different parameterization to make the ",
      "model matrix full-rank. ",
      "This may require you to choose different ",
      "terms in the model formula, choose a different informative prior ",
      "archetype, regress on fewer covariates, and/or or pool ",
      "levels of one or more factors in the data. ",
      "Otherwise, fixed effects may not be ",
      "identifiable and MCMC sampling may not converge. ",
      "Set check_rank = FALSE in brm_formula() to suppress this error."
    )
  )
}

brm_formula_new <- function(
  formula,
  brm_intercept,
  brm_baseline,
  brm_baseline_subgroup,
  brm_baseline_subgroup_time,
  brm_baseline_time,
  brm_group,
  brm_group_subgroup,
  brm_group_subgroup_time,
  brm_group_time,
  brm_subgroup,
  brm_subgroup_time,
  brm_time,
  brm_covariates,
  brm_correlation,
  brm_autoregressive_order,
  brm_moving_average_order,
  brm_residual_covariance_arma_estimation,
  brm_allow_effect_size,
  brm_model_missing_outcomes
) {
  structure(
    formula,
    class = unique(c("brms_mmrm_formula", class(formula))),
    brm_intercept = brm_intercept,
    brm_baseline = brm_baseline,
    brm_baseline_subgroup = brm_baseline_subgroup,
    brm_baseline_subgroup_time = brm_baseline_subgroup_time,
    brm_baseline_time = brm_baseline_time,
    brm_group = brm_group,
    brm_group_subgroup = brm_group_subgroup,
    brm_group_subgroup_time = brm_group_subgroup_time,
    brm_group_time = brm_group_time,
    brm_subgroup = brm_subgroup,
    brm_subgroup_time = brm_subgroup_time,
    brm_time = brm_time,
    brm_covariates = brm_covariates,
    brm_correlation = brm_correlation,
    brm_autoregressive_order = brm_autoregressive_order,
    brm_moving_average_order = brm_moving_average_order,
    brm_residual_covariance_arma_estimation =
      brm_residual_covariance_arma_estimation,
    brm_allow_effect_size = brm_allow_effect_size,
    brm_model_missing_outcomes = brm_model_missing_outcomes
  )
}

brm_formula_archetype_new <- function(
  formula,
  brm_correlation,
  brm_autoregressive_order,
  brm_moving_average_order,
  brm_residual_covariance_arma_estimation,
  brm_allow_effect_size,
  brm_model_missing_outcomes
) {
  structure(
    formula,
    class = unique(
      c("brms_mmrm_formula_archetype", "brms_mmrm_formula", class(formula))
    ),
    brm_correlation = brm_correlation,
    brm_autoregressive_order = brm_autoregressive_order,
    brm_moving_average_order = brm_moving_average_order,
    brm_residual_covariance_arma_estimation =
      brm_residual_covariance_arma_estimation,
    brm_allow_effect_size = brm_allow_effect_size,
    brm_model_missing_outcomes = brm_model_missing_outcomes
  )
}

brm_formula_validate <- function(formula) {
  UseMethod("brm_formula_validate")
}

#' @export
brm_formula_validate.default <- function(formula) {
  assert(
    formula,
    inherits(., "brms_mmrm_formula"),
    inherits(., "brmsformula"),
    message = "please use brm_formula() to create the model formula"
  )
  attributes <- c(
    "brm_intercept",
    "brm_baseline",
    "brm_baseline_subgroup",
    "brm_baseline_subgroup_time",
    "brm_baseline_time",
    "brm_group",
    "brm_group_subgroup",
    "brm_group_subgroup_time",
    "brm_group_time",
    "brm_subgroup",
    "brm_subgroup_time",
    "brm_time",
    "brm_covariates",
    "brm_allow_effect_size"
  )
  for (attribute in attributes) {
    assert_lgl(
      attr(formula, attribute),
      message = paste(attribute, "attribute must be TRUE or FALSE in formula")
    )
  }
  brm_formula_validate_covariance(formula)
}

#' @export
brm_formula_validate.brms_mmrm_formula_archetype <- function(formula) {
  assert(
    formula,
    inherits(., "brms_mmrm_formula"),
    inherits(., "brmsformula"),
    message = "please use brm_formula() to create the model formula"
  )
  attributes <- c(
    "brm_allow_effect_size"
  )
  for (attribute in attributes) {
    assert_lgl(
      attr(formula, attribute),
      message = paste(attribute, "attribute must be TRUE or FALSE in formula")
    )
  }
  brm_formula_validate_covariance(formula)
}

brm_formula_validate_covariance <- function(formula) {
  assert_lgl(
    attr(formula, "brm_residual_covariance_arma_estimation"),
    message = "residual_covariance_arma_estimation must be TRUE or FALSE"
  )
  assert(
    attr(formula, "brm_autoregressive_order"),
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "autoregressive_order must be a nonnegative integer of length 1"
  )
  assert(
    attr(formula, "brm_moving_average_order"),
    is.numeric(.),
    !anyNA(.),
    length(.) == 1L,
    . >= 0,
    message = "moving_average_order must be a nonnegative integer of length 1"
  )
  brm_formula_validate_correlation(attr(formula, "brm_correlation"))
}

brm_formula_validate_correlation <- function(correlation) {
  assert_chr(
    correlation,
    "correlation arg must be a nonempty character string"
  )
  choices <- c(
    "unstructured",
    "autoregressive_moving_average",
    "autoregressive",
    "moving_average",
    "compound_symmetry",
    "diagonal"
  )
  assert(
    correlation %in% choices,
    message = paste(
      "correlation arg must be one of:",
      paste(choices, collapse = ", ")
    )
  )
}
