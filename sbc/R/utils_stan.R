random_lkj <- function(dimension) {
  shape <- round(runif(n = 1L, min = 1, max = 1.5), 4)
  text <- sprintf("lkj_corr_cholesky(%s)", shape)
  list(text = text, shape = shape)
}

random_normal <- function() {
  mean <- round(runif(n = 1L, min = -0.25, max = 0.25), 4)
  sd <- round(runif(n = 1L, min = 0.25, max = 3), 4)
  text <- sprintf("normal(%s, %s)", mean, sd)
  list(text = text, mean = mean, sd = sd)
}

new_prior_lkj <- function(dimension) {
  lkj <- random_lkj(dimension = dimension)
  brms::set_prior(prior = lkj$text, class = "Lcortime") |>
    dplyr::mutate(
      name = "lkj",
      mean = NA_real_,
      sd = NA_real_, 
      shape = lkj$shape
    )
}

new_prior_normal <- function(
  coef = "",
  class = "b",
  dpar = ""
) {
  normal <- random_normal()
  name <- "b_"
  if (dpar == "sigma") {
    name <- paste0(name, "sigma_")
  }
  if (class == "Intercept") {
    name <- "b_Intercept"
  } else {
    name <- paste0(name, coef)
  }
  brms::set_prior(
    prior = normal$text,
    class = class,
    coef = coef,
    dpar = dpar
  ) |>
    dplyr::mutate(
      name = name,
      mean = normal$mean,
      sd = normal$sd,
      shape = NA_real_
    )
}

as_brms_prior <- function(prior) {
  dplyr::select(prior, -any_of(c("name", "mean", "sd", "shape")))
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
