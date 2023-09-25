simulate_complex <- function(prior, chains, warmup, iter) {
  options(brms.backend = "cmdstanr")
  n_group <- 2L
  n_patient <- 150L
  n_time <- 3L
  outline <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0.3,
    rate_lapse = 0.08
  ) |>
    brms.mmrm::brm_simulate_continuous(
      names = c("continuous1", "continuous2")
    ) |>
    brms.mmrm::brm_simulate_categorical(
      names = "balanced",
      levels = c("level1", "level2", "level3")
    )  |>
    brms.mmrm::brm_simulate_categorical(
      names = "unbalanced",
      levels = c("level1", "level2", "level3"),
      probabilities = c(0.64, 0.26, 0.1)
    )
  formula <- brms.mmrm::brm_formula(
    data = outline,
    intercept = TRUE,
    effect_baseline = FALSE,
    effect_group = TRUE,
    effect_time = TRUE,
    interaction_baseline = TRUE,
    interaction_group = TRUE,
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

get_prior_complex <- function() {
  prior <- brms::set_prior(prior = random_lkj(), class = "cortime") +
    brms::set_prior(prior = random_t(), class = "Intercept")
  terms <- c(
    "balancedlevel2",
    "balancedlevel3",
    "continuous1",
    "continuous2",
    "groupgroup_2",
    "timetime_2",
    "timetime_2:groupgroup_2",
    "timetime_3",
    "timetime_3:groupgroup_2",
    "unbalancedlevel2",
    "unbalancedlevel3"
  )
  for (term in terms) {
    prior <- prior + brms::set_prior(
      prior = random_t(),
      class = "b",
      coef = term
    )
  }
  terms <- c(
    "timetime_1",
    "timetime_2",
    "timetime_3"
  )
  for (term in terms) {
    prior <- prior + brms::set_prior(
      prior = random_t(),
      class = "b",
      coef = term,
      dpar = "sigma"
    )
  }
  prior
}
