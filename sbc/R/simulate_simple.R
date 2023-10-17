simulate_simple <- function(prior, chains, warmup, iter) {
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
  run_simulation(
    outline = outline,
    formula = formula,
    prior = prior,
    chains = chains,
    warmup = warmup,
    iter = iter
  )
}

get_prior_simple <- function() {
  n_group <- 3L
  n_time <- 4L
  prior <- brms::set_prior(prior = random_lkj_text(), class = "cortime")
  for (group in seq_len(n_group - 1L) + 1L) {
    prior <- prior + brms::set_prior(
      prior = random_t_text(),
      class = "b",
      coef = paste0("groupgroup_", group)
    )
  }
  for (time in seq_len(n_time)) {
    prior <- prior + brms::set_prior(
      prior = random_t_text(),
      class = "b",
      coef = paste0("timetime_", time)
    )
  }
  for (time in seq_len(n_time)) {
    prior <- prior + brms::set_prior(
      prior = random_t_text(),
      class = "b",
      coef = paste0("timetime_", time),
      dpar = "sigma"
    )
  }
  prior
}
