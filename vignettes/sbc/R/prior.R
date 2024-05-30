setup_prior <- function(scenario) {
  setup <- scenario()
  random_prior(data = setup$data, formula = setup$formula)
}

random_prior <- function(data, formula) {
  n_time <- length(unique(data[[attr(data, "brm_time")]]))
  prior <- brms::get_prior(data = data, formula = formula)
  prior$r <- NA_character_
  is_fixed_effect <- prior$class == "Intercept" |
    (prior$class == "b" & nzchar(prior$coef))
  for (index in which(is_fixed_effect)) {
    normal <- random_normal()
    prior$prior[index] <- normal$stan
    prior$r[index] <- normal$r
  }
  if ("cortime" %in% prior$class) {
    lkj <- random_lkj(dimension = n_time)
    prior$prior[prior$class == "cortime"] <- lkj$stan
    prior$r[prior$class == "cortime"] <- lkj$r
  }
  if ("ar" %in% prior$class) {
    beta <- random_beta()
    prior$prior[prior$class == "ar"] <- beta$stan
    prior$r[prior$class == "ar"] <- beta$r
    prior$lb[prior$class == "ar"] <- 0
    prior$ub[prior$class == "ar"] <- 1
  }
  if ("cosy" %in% prior$class) {
    beta <- random_beta()
    prior$prior[prior$class == "cosy"] <- beta$stan
    prior$r[prior$class == "cosy"] <- beta$r
  }
  prior[!is.na(prior$r), ]
}

random_beta <- function() {
  alpha <- round(runif(n = 1L, min = 0.5, max = 2), 4)
  beta <- round(runif(n = 1L, min = 0.5, max = 2), 4)
  stan <- sprintf("beta(%s, %s)", alpha, beta)
  r <- sprintf(
    "stats::rbeta(n = 1L, shape1 = %s, shape2 = %s)",
    alpha,
    beta
  )
  list(stan = stan, r = r)
}

random_normal <- function() {
  mean <- round(runif(n = 1L, min = -0.25, max = 0.25), 4)
  sd <- round(runif(n = 1L, min = 0.25, max = 3), 4)
  stan <- sprintf("normal(%s, %s)", mean, sd)
  r <- sprintf("stats::rnorm(n = 1L, mean = %s, sd = %s)", mean, sd)
  list(stan = stan, r = r)
}

random_lkj <- function(dimension) {
  shape <- round(runif(n = 1L, min = 1, max = 1.5), 4)
  stan <- sprintf("lkj(%s)", shape)
  r <- sprintf(
    "trialr::rlkjcorr(n = 1L, K = %s, eta = %s)",
    dimension,
    shape
  )
  list(stan = stan, r = r)
}

as_brms_prior <- function(prior) {
  dplyr::select(prior, -any_of(c("r")))
}

assert_equal_priors <- function(prior1, prior2) {
  prior1 <- dplyr::filter(prior1, !(prior == "(flat)" | source == "default"))
  prior2 <- dplyr::filter(prior2, !(prior == "(flat)" | source == "default"))
  stopifnot(!anyDuplicated(prior1$prior))
  stopifnot(!anyDuplicated(prior2$prior))
  prior1 <- dplyr::arrange(prior1, prior)
  prior2 <- dplyr::arrange(prior2, prior)
  stopifnot(all(colnames(prior1) == colnames(prior2)))
  for (name in c("lb", "ub")) {
    prior1[[name]] <- NULL
    prior2[[name]] <- NULL
  }
  for (name in colnames(prior1)) {
    stopifnot(all(prior1[[name]] == prior2[[name]]))
  }
}
