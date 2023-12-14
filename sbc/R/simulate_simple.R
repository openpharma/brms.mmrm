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
  prior <- new_prior_lkj(dimension = n_time)
  terms <- c(
    "groupgroup_2",
    "groupgroup_3",
    "timetime_1",
    "timetime_2",
    "timetime_3",
    "timetime_4"
  )
  for (term in terms) {
    prior <- prior + new_prior_normal(coef = term)
  }
  terms <- c(
    "timetime_1",
    "timetime_2",
    "timetime_3",
    "timetime_4"
  )
  for (term in terms) {
    prior <- prior + new_prior_normal(coef = term, dpar = "sigma")
  }
  prior
}
