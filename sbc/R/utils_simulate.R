run_simulation <- function(
  outline = outline,
  formula = formula,
  prior = prior,
  chains = chains,
  warmup = warmup,
  iter = iter
) {
  rnorm(1)
  seed <- .GlobalEnv[[".Random.seed"]]
  simulation <- simulate_response(
    outline = outline,
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
  list(
    seed = seed,
    simulation = simulation,
    draws = posterior::as_draws_matrix(model),
    ranks = get_sbc_ranks(model, simulation)
  )
}

bind_ranks <- function(results) {
  out <- purrr::map(results, ~.x[[1L]]$ranks)
  dplyr::bind_rows(out)
}

simulate_response <- function(outline, formula, prior) {
  data <- outline
  data$response <- seq_len(nrow(data))
  stan_data <- brms::make_standata(
    formula = formula,
    data = data,
    prior = as_brms_prior(prior)
  )
  brms_permutation <- match(x = stan_data$Y, table = data$response)
  undo_brms_permutation <- match(x = data$response, table = stan_data$Y)
  stopifnot(all(stan_data$Y[undo_brms_permutation] == data$response))
  model_matrix <- stan_data$X[undo_brms_permutation, ]
  abridged_formula <- paste("~", as.character(formula[[1L]])[3]) |>
    gsub(pattern = " + unstr(time = time, gr = patient)", replacement = "", fixed = TRUE) |>
    as.formula()
  stopifnot(
    all(
      as.matrix(model_matrix) ==
        as.matrix(model.matrix(abridged_formula, data = data))
    )
  )
  prior$coef[prior$class == "Intercept"] <- "Intercept"
  stopifnot(all(sort(colnames(model_matrix)) %in% prior$coef))
  prior_beta <- dplyr::filter(
    prior,
    class %in% c("b", "Intercept"),
    dpar != "sigma"
  )
  prior_beta <- prior_beta[match(x = colnames(model_matrix), table = prior_beta$coef), ]
  stopifnot(all(prior_beta$coef == colnames(model_matrix)))
  n_beta <- nrow(prior_beta)
  beta <- stats::rnorm(n = n_beta, mean = prior_beta$mean, sd = prior_beta$sd)
  names(beta) <- prior_beta$coef
  stopifnot(all(sort(names(beta)) == sort(names(model_matrix))))
  stopifnot(!anyNA(names(beta)))
  stopifnot(!anyNA(beta))
  beta <- beta[colnames(model_matrix)]
  names(beta) <- prior_beta$name
  stopifnot(!anyNA(names(beta)))
  stopifnot(!anyNA(beta))
  prior_sigma <- dplyr::arrange(dplyr::filter(prior, dpar == "sigma"), coef)
  n_time <- nrow(prior_sigma)
  b_sigma <- stats::rnorm(n_time, mean = prior_sigma$mean, sd = prior_sigma$sd)
  names(b_sigma) <- prior_sigma$name
  sigma <- exp(b_sigma)
  shape <- prior[prior$class == "Lcortime", "shape"]
  correlation <- trialr::rlkjcorr(n = 1L, K = n_time, eta = shape)
  i <- rep(seq_len(n_time), each = n_time)
  j <- rep(seq_len(n_time), times = n_time)
  cortime <- as.numeric(correlation)[j > i]
  names(cortime) <- sprintf("cortime__time_%s__time_%s", i[j > i], j[j > i])
  covariance <- diag(sigma) %*% correlation %*% diag(sigma)
  n_patient <- nrow(data) / n_time
  stopifnot(
    all(data$time == paste0("time_", rep(seq_len(n_time), times = n_patient)))
  )
  labels_group <- data$group
  labels_time <- data$time
  labels_patient <- data$patient
  data <- data |>
    dplyr::mutate(
      mu = as.numeric(model_matrix %*% beta),
      index_patient = rep(seq_len(n_patient), each = n_time)
    ) |>
    dplyr::group_by(index_patient) |>
    dplyr::mutate(response = MASS::mvrnorm(mu = mu, Sigma = covariance)) |>
    dplyr::ungroup() |>
    dplyr::select(-index_patient, -mu)
  data$response[data$missing] <- NA_real_
  stopifnot(all(colnames(data) == colnames(outline)))
  stopifnot(all(data$group == labels_group))
  stopifnot(all(data$time == labels_time))
  stopifnot(all(data$patient == labels_patient))
  attributes(data) <- attributes(outline)
  parameters <- c(beta, b_sigma, cortime)
  stopifnot(!anyDuplicated(names(parameters)))
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
