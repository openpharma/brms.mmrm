run_simulation <- function(
  outline = outline,
  formula = formula,
  prior = prior,
  chains = chains,
  warmup = warmup,
  iter = iter
) {
  simulation <- simulate_response(
    data = outline,
    formula = formula,
    prior = prior
  )
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

simulate_response <- function(data, formula, prior) {
  name_intercept <- "Intercept"
  data$response <- 0
  stan_data <- brms::make_standata(
    formula = formula,
    data = data,
    prior = prior
  )
  model_matrix <- stan_data$X
  beta <- numeric(0L)
  for (label in setdiff(colnames(model_matrix), name_intercept)) {
    text <- dplyr::filter(prior, coef == label, class == "b", dpar == "")$prior
    beta <- c(beta, structure(eval(parse(text = text)), names = label))
  }
  if (name_intercept %in% prior$class) {
    text <- dplyr::filter(prior, class == name_intercept)$prior
    beta <- c(beta, Intercept = eval(parse(text = text)))
  }
  beta <- beta[colnames(model_matrix)]
  stopifnot(!anyNA(beta))
  mu <- as.numeric(model_matrix %*% beta)
  names(beta) <- paste0("b_", names(beta))
  prior_sigma <- dplyr::filter(prior, dpar == "sigma")
  b_sigma <- numeric(0L)
  for (label in prior_sigma$coef) {
    text <- dplyr::filter(prior, coef == label, dpar == "sigma")$prior
    name <- paste0("b_sigma_", label)
    b_sigma <- c(b_sigma, structure(eval(parse(text = text)), names = name))
  }
  sigma <- exp(b_sigma)
  shape <- eval(parse(text = dplyr::filter(prior, class == "cortime")$prior))
  correlation <- trialr::rlkjcorr(n = 1L, K = length(sigma), eta = shape)
  cortime <- numeric(0L)
  for (i in seq(from = 1L, to = length(sigma))) {
    for (j in seq(from = 1L, to = length(sigma))) {
      if (j > i) {
        name <- sprintf("cortime__time_%s__time_%s", i, j)
        cortime <- c(cortime, structure(correlation[i, j], names = name))
      }
    }
  }
  covariance <- diag(sigma) %*% correlation %*% diag(sigma)
  for (patient in unique(data$patient)) {
    index <- which(data$patient == patient)
    mu_patient <- mu[index]
    data$response[index] <- as.numeric(
      MASS::mvrnorm(
        n = 1L,
        mu = mu_patient,
        Sigma = covariance
      )
    )
  }
  data$response[data$missing] <- NA_real_
  parameters <- c(beta, b_sigma, cortime)
  list(data = data, parameters = parameters)
}

get_sbc_ranks <- function(model, simulation) {
  draws <- posterior::as_draws_matrix(model)
  draws <- draws[, setdiff(colnames(draws), c("lprior", "lp__"))]
  truth <- simulation$parameters
  stopifnot(all(sort(names(truth)) == sort(colnames(draws))))
  draws <- draws[, names(truth)]
  ranks <- SBC::calculate_ranks_draws_matrix(variables = truth, dm = draws)
  tibble::as_tibble(as.list(ranks))
}
