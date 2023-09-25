simulate_simple <- function(prior, chains, warmup, iter) {
  options(brms.backend = "cmdstanr")
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
  model <- brms.mmrm::brm_model(
    data = simulation$data,
    formula = formula,
    prior = prior,
    chains = chains,
    cores = chains,
    iter = iter,
    warmup = warmup,
  )
  draws <- posterior::as_draws_matrix(model)[, colnames(parameters)]
  ranks <- SBC::calculate_ranks_draws_matrix(variables = truth, dm = draws)
  tibble::as_tibble(as.list(ranks))
}

get_prior_simple <- function() {
  n_group <- 3L
  n_time <- 4L
  prior <- brms::set_prior(prior = random_lkj(), class = "cortime")
  for (group in seq_len(n_group - 1L) + 1L) {
    prior <- prior + brms::set_prior(
      prior = random_t(),
      class = "b",
      coef = paste0("groupgroup_", group)
    )
  }
  for (time in seq_len(n_time)) {
    prior <- prior + brms::set_prior(
      prior = random_t(),
      class = "b",
      coef = paste0("timetime_", time)
    )
  }
  for (time in seq_len(n_time)) {
    prior <- prior + brms::set_prior(
      prior = random_t(),
      class = "b",
      coef = paste0("timetime_", time),
      dpar = "sigma"
    )
  }
  prior
}
