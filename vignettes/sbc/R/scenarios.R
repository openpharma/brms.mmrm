subgroup <- function() {
  n_group <- 2L
  n_subgroup <- 2L
  n_patient <- 150L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_subgroup = n_subgroup,
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
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = TRUE,
    group = TRUE,
    time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    covariates = TRUE,
    correlation = "unstructured"
  )
  list(data = data, formula = formula, simulate = simulate_unstructured)
}

unstructured <- function() {
  n_group <- 3L
  n_patient <- 100L
  n_time <- 4L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "unstructured"
  )
  list(data = data, formula = formula, simulate = simulate_unstructured)
}

autoregressive_moving_average <- function() {
  n_group <- 2L
  n_patient <- 100L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "autoregressive_moving_average",
    autoregressive_order = 1L,
    moving_average_order = 1L
  )
  list(data = data, formula = formula, simulate = simulate_arma11)
}

autoregressive <- function() {
  n_group <- 2L
  n_patient <- 100L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "autoregressive",
    autoregressive_order = 2L
  )
  list(data = data, formula = formula, simulate = simulate_ar2)
}

moving_average <- function() {
  n_group <- 2L
  n_patient <- 100L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "moving_average",
    moving_average_order = 2L
  )
  list(data = data, formula = formula, simulate = simulate_ma2)
}

compound_symmetry <- function() {
  n_group <- 2L
  n_patient <- 100L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "compound_symmetry"
  )
  list(data = data, formula = formula, simulate = simulate_compound_symmetry)
}

diagonal <- function() {
  n_group <- 2L
  n_patient <- 100L
  n_time <- 3L
  data <- brms.mmrm::brm_simulate_outline(
    n_group = n_group,
    n_patient = n_patient,
    n_time = n_time,
    rate_dropout = 0,
    rate_lapse = 0
  )
  data[[attr(data, "brm_outcome")]] <- seq_len(nrow(data))
  formula <- brms.mmrm::brm_formula(
    data = data,
    intercept = FALSE,
    baseline = FALSE,
    group = TRUE,
    time = TRUE,
    baseline_time = FALSE,
    group_time = FALSE,
    covariates = FALSE,
    correlation = "diagonal",
    sigma = brms.mmrm::brm_formula_sigma(
      data,
      intercept = TRUE,
      group = TRUE,
      group_time = TRUE,
      time = TRUE
    )
  )
  list(data = data, formula = formula, simulate = simulate_diagonal)
}
