#' @title Summarize an MMRM.
#' @export
#' @family results
#' @description Summarize a basic MMRM model fit.
#' @return A `tibble` with summary statistics of the marginal posterior.
#' @inheritParams brm_formula
#' @param model Fitted `brms` model object from [brm_model()]
#' @examples
#' set.seed(0L)
#' sim <- brm_simulate()
#' data <- sim$data
#' formula <- brm_formula(
#'   response = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   effect_base = FALSE,
#'   interaction_base = FALSE
#' )
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = data,
#'         formula = formula,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' brm_summary(
#'   model = model,
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   control = 1
#' )
brm_summary <- function(
  model,
  base = "BASE",
  group = "TRT01P",
  time = "AVISIT",
  patient = "USUBJID",
  covariates = character(0),
  control = "Placebo"
) {
  assert_chr(base, "base arg must be a nonempty character string")
  assert_chr(group, "group arg must be a nonempty character string")
  assert_chr(time, "time arg must be a nonempty character string")
  assert_chr(patient, "patient arg must be a nonempty character string")
  assert_chr_vec(covariates, "covariates arg must be a character vector")
  assert(
    control,
    is.atomic(.),
    length(.) == 1L,
    !anyNA(.),
    message = "control arg must be a length-1 non-missing atomic value"
  )
  assert(is.data.frame(model$data))
  data <- model$data
  assert(
    group %in% colnames(data),
    message = "group arg must be a data column name"
  )
  assert(
    time %in% colnames(data),
    message = "time arg must be a data column name"
  )
  assert(
    patient %in% colnames(data),
    message = "patient arg must be a data column name"
  )
  assert(
    covariates %in% colnames(data),
    message = "all covariates must be data column names"
  )
  assert(
    control %in% data[[group]],
    message = "control arg must be in data[[group]]"
  )
  nuisance <- c(base, patient, covariates)
  emmeans_response <- emmeans::emmeans(
    object = model,
    specs = as.formula(sprintf("~%s:%s", time, group)),
    weights = "proportional",
    nuisance = nuisance
  )
  table_response <- brm_summary_response(
    data = data,
    emmeans_response = emmeans_response
  )
  table_diff <- brm_summary_diff_change(
    data = data,
    emmeans_response = emmeans_response,
    group = group,
    time = time,
    nuisance = nuisance,
    control = control
  )
  dplyr::left_join(
    x = table_response,
    y = table_diff,
    by = c(group, time)
  )
}

brm_summary_response <- function(data, emmeans_response) {
  out <- tibble::as_tibble(emmeans_response)
  out$response_mean <- out$emmean
  out$response_lower <- out$lower.HPD
  out$response_upper <- out$upper.HPD
  out$emmean <- NULL
  out$lower.HPD <- NULL
  out$upper.HPD <- NULL
  out
}

brm_summary_diff_change <- function(
  data,
  emmeans_response,
  group,
  time,
  nuisance,
  control
) {
  contrasts_diff <- list()
  reference <- tibble::as_tibble(emmeans_response)
  for (level_group in setdiff(sort(unique(reference[[group]])), control)) {
    for (level_time in sort(unique(reference[[time]]))) {
      contrast_treatment <- as.integer(
        reference[[group]] == group & reference[[time]] == level_time
      )
      contrast_control <- as.integer(
        reference[[group]] == control & reference[[time]] == level_time
      )
      contrast <- contrast_treatment - contrast_control
      contrasts_diff[[length(contrasts_diff) + 1L]] <- contrast
    }
  }
  emmeans_diff <- emmeans::contrast(
    emmeans_response,
    method = contrasts_diff,
    adjust = "none",
    nuisance = nuisance
  )
  subset_reference <- reference[reference[[group]] != control, ]
  out <- tibble::as_tibble(emmeans_diff)
  out[[group]] <- subset_reference[[group]]
  out[[time]] <- subset_reference[[time]]
  out$contrast <- NULL
  out$diff_mean <- out$estimate
  out$diff_lower <- out$lower.HPD
  out$diff_upper <- out$upper.HPD
  out$estimate <- NULL
  out$lower.HPD <- NULL
  out$upper.HPD <- NULL
  out
}
