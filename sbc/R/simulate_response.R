simulate_response <- function(data, formula, prior) {
  UseMethod("simulate_response")
}

simulate_response.unstructured <- function(data, formula, prior) {
  beta <- simulate_beta(data = data, formula = formula, prior = prior)
  x_beta <- derive_x_beta(
    data = data,
    formula = formula,
    prior = prior,
    beta = beta
  )
  b_sigma <- simulate_b_sigma(data = data, formula = formula, prior = prior)
  sigma <- derive_sigma(
    data = data,
    formula = formula,
    prior = prior,
    b_sigma = b_sigma
  )
  n_time <- length(unique(data[[attr(data, "brm_time")]]))
  n_patient <- nrow(data) / n_time
  shape <- prior[prior$class == "Lcortime", "shape"]
  correlation <- trialr::rlkjcorr(n = 1L, K = n_time, eta = shape)
  i <- rep(seq_len(n_time), each = n_time)
  j <- rep(seq_len(n_time), times = n_time)
  cortime <- as.numeric(correlation)[j > i]
  names(cortime) <- sprintf("cortime__time_%s__time_%s", i[j > i], j[j > i])
  for (patient in seq_len(n_patient)) {
    rows <- seq_len(n_time) + n_time * (patient - 1L)
    covariance <- diag(sigma[rows]) %*% correlation %*% diag(sigma[rows])
    response <- MASS::mvrnorm(mu = x_beta[rows], Sigma = covariance)
    data[[attr(data, "brm_outcome")]][rows] <- response
  }
  data$response[data$missing] <- NA_real_
  parameters <- c(beta, b_sigma, cortime)
  list(data = data, parameters = parameters)
}

simulate_beta <- function(data, formula, prior, model_matrix) {
  model_matrix <- get_model_matrix(
    data = data,
    formula = formula,
    prior = prior,
    which = "X"
  )
  abridged_formula <- diagonal_formula(data = data, formula = formula)[[1L]]
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
  index <- match(x = colnames(model_matrix), table = prior_beta$coef)
  prior_beta <- prior_beta[index, ]
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
  beta
}

derive_x_beta <- function(data, formula, prior, beta) {
  model_matrix <- get_model_matrix(
    data = data,
    formula = formula,
    prior = prior,
    which = "X"
  )
  as.numeric(model_matrix %*% beta)
}

simulate_b_sigma <- function(data, formula, prior) {
  prior_b_sigma <- dplyr::arrange(dplyr::filter(prior, dpar == "sigma"), coef)
  b_sigma <- stats::rnorm(
    n = nrow(prior_b_sigma),
    mean = prior_b_sigma$mean,
    sd = prior_b_sigma$sd
  )
  names(b_sigma) <- prior_b_sigma$name
  b_sigma
}

derive_sigma <- function(data, formula, prior, b_sigma) {
  model_matrix <- get_model_matrix(
    data = data,
    formula = formula,
    prior = prior,
    which = "X_sigma"
  )
  stopifnot(all(names(b_sigma) == paste0("b_sigma_", colnames(model_matrix))))
  as.numeric(exp(model_matrix %*% b_sigma))
}

get_model_matrix <- function(data, formula, prior, which = "X") {
  data$response <- seq_len(nrow(data))
  stan_data <- brms::make_standata(
    formula = formula,
    data = data,
    prior = as_brms_prior(prior)
  )
  brms_permutation <- match(x = stan_data$Y, table = data$response)
  undo_brms_permutation <- match(x = data$response, table = stan_data$Y)
  stopifnot(all(stan_data$Y[undo_brms_permutation] == data$response))
  model_matrix <- stan_data[[which]][undo_brms_permutation, ]
}

diagonal_formula <- function(data, formula) {
  args <- attributes(formula)
  args <- args[grepl(pattern = "^brm_", x = names(args))]
  names(args) <- gsub(pattern = "^brm_", replacement = "", x = names(args))
  args$data <- data
  args$correlation <- "diagonal"
  do.call(what = brms.mmrm::brm_formula, args = args)
}
