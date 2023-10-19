simulate_benchmark_simple <- function(prior, chains, warmup, iter) {
  n_group <- 3L
  n_patient <- 100L
  n_time <- 4L
  outline <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  formula <- brms.mmrm::brm_formula(
    data = outline,
    intercept = FALSE,
    effect_baseline = FALSE,
    effect_group = TRUE,
    effect_time = TRUE,
    interaction_baseline = FALSE,
    interaction_group = FALSE,
    correlation = "unstructured"
  )
  simulation <- brms.mmrm::brm_simulate_prior(
    data = outline,
    formula = formula,
    prior = prior,
    chains = chains,
    cores = chains,
    iter = iter,
    warmup = warmup,
    refresh = 0L
  )
  parameters <- dplyr::select(
    tibble::as_tibble(simulation$parameters),
    tidyselect::starts_with("b_"),
    tidyselect::starts_with("cortime_")
  )
  truth <- as.numeric(parameters[nrow(parameters), ])
  names(truth) <- names(parameters)
  simulation$parameters <- truth
  options(brms.backend = "rstan")
  model <- brms.mmrm::brm_model(
    data = simulation$data,
    formula = formula,
    prior = prior,
    chains = chains,
    cores = chains,
    iter = iter,
    warmup = warmup,
  )
  get_sbc_ranks(model, simulation)
}