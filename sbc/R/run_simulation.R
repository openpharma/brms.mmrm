run_simulation <- function(
  scenario,
  prior,
  chains,
  warmup,
  iter
) {
  setup <- scenario()
  data <- setup$data
  formula <- setup$formula
  rnorm(1)
  seed <- .GlobalEnv[[".Random.seed"]]
  class(data) <- c(attr(formula, "brm_correlation"), class(data))
  simulation <- simulate_response(
    data = data,
    formula = formula,
    prior = prior
  )
  options(brms.backend = "rstan")
  model <- brms.mmrm::brm_model(
    data = dplyr::filter(simulation$data, !is.na(response)),
    formula = formula,
    prior = as_brms_prior(prior),
    chains = chains,
    cores = chains,
    iter = iter,
    warmup = warmup,
  )
  assert_equal_priors(as_brms_prior(prior), brms::prior_summary(model))
  get_sbc_ranks(model, simulation)
}

get_sbc_ranks <- function(model, simulation) {
  draws <- posterior::as_draws_matrix(model)
  draws <- draws[, setdiff(colnames(draws), c("lprior", "lp__"))]
  truth <- simulation$parameters
  if (all(c("b_Intercept", "Intercept") %in% colnames(draws))) {
    draws <- draws[, setdiff(colnames(draws), "Intercept")]
  }
  stopifnot(all(sort(names(truth)) == sort(colnames(draws))))
  draws <- draws[, names(truth)]
  ranks <- SBC::calculate_ranks_draws_matrix(variables = truth, dm = draws)
  tibble::as_tibble(as.list(ranks))
}
