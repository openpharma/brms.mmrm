test_that("brm_transform_marginal(), response, non-subgroup", {
  set.seed(0L)
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  raw_data <- fev_data
  raw_data$FEV1[is.na(raw_data$FEV1)] <- rnorm(n = sum(is.na(raw_data$FEV1)))
  grid <- dplyr::distinct(raw_data, ARMCD, AVISIT)
  grid <- dplyr::arrange(grid, ARMCD, AVISIT)
  row_names <- paste(grid$ARMCD, grid$AVISIT, sep = brm_sep())
  data <- brm_data(
    data = raw_data,
    outcome = "FEV1",
    group = "ARMCD",
    time = "AVISIT",
    patient = "USUBJID",
    reference_group = "PBO",
    reference_time = "VIS1"
  )
  formula <- brm_formula(
    data = data,
    intercept = FALSE,
    group = TRUE,
    group_time = FALSE,
    time = TRUE
  )
  formula_lm <- brm_formula(
    data = data,
    intercept = FALSE,
    group = TRUE,
    group_time = FALSE,
    time = TRUE,
    correlation = "diagonal"
  )[[1L]]
  suppressMessages(
    brm_transform_marginal(
      data = data,
      formula = formula,
      prefix = ""
    )
  )
  for (average_within_subgroup in c(TRUE, FALSE)) {
    transform <- brm_transform_marginal(
      data = data,
      formula = formula,
      prefix = "",
      average_within_subgroup = average_within_subgroup
    )
    suppressMessages(
      expect_true(is.character(summary(transform, message = TRUE)))
    )
    expect_true(is.character(summary(transform, message = FALSE)))
    model_matrix <- brms::make_standata(
      formula = formula,
      data = dplyr::mutate(data, FEV1 = 0)
    )$X
    expect_equal(colnames(transform), colnames(model_matrix))
    expect_equal(rownames(transform), row_names)
    skip_if_not_installed("emmeans")
    old_sep <- emmeans::get_emm_option("sep")
    on.exit(emmeans::emm_options(sep = old_sep))
    emmeans::emm_options(sep = brm_sep())
    model <- lm(formula = formula_lm, data = data)
    mle <- coef(model)
    names(mle) <- gsub("\\(|\\)| ", "", names(mle))
    mle <- mle[colnames(transform)]
    out <- t(transform %*% mle)
    names <- colnames(out)
    out <- setNames(as.numeric(out), names)
    exp <- emmeans::emmeans(
      object = model,
      specs = ~ARMCD:AVISIT,
      weights = "proportional",
      nuisance = c("RACE", "SEX", "WEIGHT")
    )
    exp <- as.data.frame(exp)
    exp <- setNames(exp$emmean, paste0(exp$ARMCD, brm_sep(), exp$AVISIT))
    exp <- exp[names(out)]
    expect_equal(out, exp)
  }
})

test_that("brm_transform_marginal(), change, non-subgroup", {
  set.seed(0L)
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  raw_data <- fev_data
  raw_data$FEV1[is.na(raw_data$FEV1)] <- rnorm(n = sum(is.na(raw_data$FEV1)))
  raw_data$FEV1_CHG <- raw_data$FEV1 - raw_data$FEV1_BL
  grid <- dplyr::distinct(raw_data, ARMCD, AVISIT)
  grid <- dplyr::arrange(grid, ARMCD, AVISIT)
  row_names <- paste(grid$ARMCD, grid$AVISIT, sep = brm_sep())
  data <- brm_data(
    data = raw_data,
    outcome = "FEV1_CHG",
    group = "ARMCD",
    time = "AVISIT",
    patient = "USUBJID",
    baseline = "FEV1_BL",
    reference_group = "PBO",
    covariates = c("RACE", "SEX", "WEIGHT")
  )
  formula <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    covariates = TRUE
  )
  formula_lm <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_time = TRUE,
    time = TRUE,
    covariates = TRUE,
    correlation = "diagonal"
  )[[1L]]
  for (average_within_subgroup in c(TRUE, FALSE)) {
    transform <- brm_transform_marginal(
      data = data,
      formula = formula,
      prefix = "",
      average_within_subgroup = average_within_subgroup
    )
    model_matrix <- brms::make_standata(
      formula = formula,
      data = dplyr::mutate(data, FEV1_CHG = 0)
    )$X
    expect_equal(colnames(transform), colnames(model_matrix))
    expect_equal(rownames(transform), row_names)
    skip_if_not_installed("emmeans")
    old_sep <- emmeans::get_emm_option("sep")
    on.exit(emmeans::emm_options(sep = old_sep))
    emmeans::emm_options(sep = brm_sep())
    model <- lm(formula = formula_lm, data = data)
    mle <- coef(model)
    names(mle) <- gsub("\\(|\\)| ", "", names(mle))
    names(mle)[names(mle) == "AVISITVIS2:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS2"
    names(mle)[names(mle) == "AVISITVIS3:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS3"
    names(mle)[names(mle) == "AVISITVIS4:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS4"
    mle <- mle[colnames(transform)]
    out <- t(transform %*% mle)
    names <- colnames(out)
    out <- setNames(as.numeric(out), names)
    exp <- emmeans::emmeans(
      object = model,
      specs = ~ARMCD:AVISIT,
      weights = "proportional",
      nuisance = c("RACE", "SEX", "WEIGHT")
    )
    exp <- as.data.frame(exp)
    exp <- setNames(exp$emmean, paste0(exp$ARMCD, brm_sep(), exp$AVISIT))
    exp <- exp[names(out)]
    expect_equal(out, exp)
  }
})

test_that("brm_transform_marginal(), change, subgroup, global", {
  set.seed(0L)
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  raw_data <- fev_data
  raw_data$FEV1[is.na(raw_data$FEV1)] <- rnorm(n = sum(is.na(raw_data$FEV1)))
  raw_data$FEV1_CHG <- raw_data$FEV1 - raw_data$FEV1_BL
  raw_data$ARMCD <- as.character(raw_data$ARMCD)
  raw_data$SEX <- as.character(raw_data$SEX)
  raw_data$AVISIT <- as.character(raw_data$AVISIT)
  grid <- dplyr::distinct(raw_data, ARMCD, SEX, AVISIT)
  grid <- dplyr::arrange(grid, ARMCD, SEX, AVISIT)
  row_names <- paste(grid$ARMCD, grid$SEX, grid$AVISIT, sep = brm_sep())
  data <- brm_data(
    data = raw_data,
    outcome = "FEV1_CHG",
    group = "ARMCD",
    subgroup = "SEX",
    time = "AVISIT",
    patient = "USUBJID",
    baseline = "FEV1_BL",
    reference_group = "PBO",
    reference_subgroup = "Female",
    covariates = c("RACE", "WEIGHT")
  )
  formula <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE,
    covariates = TRUE
  )
  formula_lm <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE,
    covariates = TRUE,
    correlation = "diagonal"
  )[[1L]]
  transform <- brm_transform_marginal(
    data = data,
    formula = formula,
    prefix = "",
    average_within_subgroup = FALSE
  )
  names <- c("FEV1_BL", "WEIGHT", "RACEBlackorAfricanAmerican", "RACEWhite")
  for (name in names) {
    expect_equal(length(unique(transform[, name])), 1L)
  }
  model_matrix <- brms::make_standata(formula = formula, data = data)$X
  expect_equal(colnames(transform), colnames(model_matrix))
  expect_equal(rownames(transform), row_names)
  skip_if_not_installed("emmeans")
  old_sep <- emmeans::get_emm_option("sep")
  on.exit(emmeans::emm_options(sep = old_sep))
  emmeans::emm_options(sep = brm_sep())
  model <- lm(formula = formula_lm, data = data)
  mle <- coef(model)
  names(mle) <- gsub("\\(|\\)| ", "", names(mle))
  names(mle)[names(mle) == "SEXMale:ARMCDTRT"] <- "ARMCDTRT:SEXMale"
  names(mle)[names(mle) == "AVISITVIS2:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS2"
  names(mle)[names(mle) == "AVISITVIS3:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS3"
  names(mle)[names(mle) == "AVISITVIS4:ARMCDTRT"] <- "ARMCDTRT:AVISITVIS4"
  names(mle)[names(mle) == "SEXMale:AVISITVIS2:ARMCDTRT"] <-
    "ARMCDTRT:SEXMale:AVISITVIS2"
  names(mle)[names(mle) == "SEXMale:AVISITVIS3:ARMCDTRT"] <-
    "ARMCDTRT:SEXMale:AVISITVIS3"
  names(mle)[names(mle) == "SEXMale:AVISITVIS4:ARMCDTRT"] <-
    "ARMCDTRT:SEXMale:AVISITVIS4"
  mle <- mle[colnames(transform)]
  out <- t(transform %*% mle)
  names <- colnames(out)
  out <- setNames(as.numeric(out), names)
  exp <- emmeans::emmeans(
    object = model,
    specs = ~ARMCD:SEX:AVISIT,
    weights = "proportional",
    nuisance = c("RACE", "WEIGHT")
  )
  exp <- as.data.frame(exp)
  exp <- setNames(
    exp$emmean,
    paste(exp$ARMCD, exp$SEX, exp$AVISIT, sep = brm_sep())
  )
  exp <- exp[names(out)]
  expect_equal(out, exp)
})

test_that("brm_transform_marginal(), change, subgroup, local", {
  set.seed(0L)
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  raw_data <- fev_data
  raw_data$FEV1[is.na(raw_data$FEV1)] <- rnorm(n = sum(is.na(raw_data$FEV1)))
  raw_data$FEV1_CHG <- raw_data$FEV1 - raw_data$FEV1_BL
  raw_data$ARMCD <- as.character(raw_data$ARMCD)
  raw_data$SEX <- as.character(raw_data$SEX)
  raw_data$AVISIT <- as.character(raw_data$AVISIT)
  grid <- dplyr::distinct(raw_data, ARMCD, SEX, AVISIT)
  grid <- dplyr::arrange(grid, ARMCD, SEX, AVISIT)
  row_names <- paste(grid$ARMCD, grid$SEX, grid$AVISIT, sep = brm_sep())
  data <- brm_data(
    data = raw_data,
    outcome = "FEV1_CHG",
    group = "ARMCD",
    subgroup = "SEX",
    time = "AVISIT",
    patient = "USUBJID",
    baseline = "FEV1_BL",
    reference_group = "PBO",
    reference_subgroup = "Female",
    covariates = c("RACE", "WEIGHT")
  )
  formula <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE,
    covariates = TRUE
  )
  formula_lm <- brm_formula(
    data = data,
    intercept = TRUE,
    baseline = TRUE,
    baseline_subgroup = TRUE,
    baseline_subgroup_time = TRUE,
    baseline_time = TRUE,
    group = TRUE,
    group_subgroup = TRUE,
    group_subgroup_time = TRUE,
    group_time = TRUE,
    subgroup = TRUE,
    subgroup_time = TRUE,
    time = TRUE,
    covariates = TRUE,
    correlation = "diagonal"
  )[[1L]]
  transform <- brm_transform_marginal(
    data = data,
    formula = formula,
    prefix = "",
    average_within_subgroup = TRUE
  )
  model_matrix <- brms::make_standata(formula = formula_lm, data = data)$X
  expect_equal(colnames(transform), colnames(model_matrix))
  expect_equal(rownames(transform), row_names)
  names <- c("FEV1_BL", "WEIGHT", "RACEBlackorAfricanAmerican", "RACEWhite")
  for (name in names) {
    length(unique(transform[, name])) == 2L
    expect_equal(
      transform["PBO|Female|VIS1", name],
      mean(model_matrix[, name][data$SEX == "Female"])
    )
    expect_equal(
      transform["PBO|Male|VIS1", name],
      mean(model_matrix[, name][data$SEX == "Male"])
    )
  }
})

test_that("archetype non-subgroup no nuisance", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n()))
  scenario <- brm_archetype_successive_cells(data)
  transform <- brm_transform_marginal(
    data = scenario,
    formula = brm_formula(scenario)
  )
  expect_equal(
    rownames(transform),
    c(
      "group_1|time_1",
      "group_1|time_2",
      "group_1|time_3",
      "group_2|time_1",
      "group_2|time_2",
      "group_2|time_3"
    )
  )
  expect_equal(
    sort(colnames(transform)),
    sort(
      c(
        "b_x_group_1_time_1",
        "b_x_group_1_time_2",
        "b_x_group_1_time_3",
        "b_x_group_2_time_1",
        "b_x_group_2_time_2",
        "b_x_group_2_time_3"
      )
    )
  )
  expect_equal(
    unclass(unname(transform)),
    kronecker(diag(2), diag(3) + lower.tri(diag(3)))
  )
  terms <- c("response ~ 0", grep("^x_", colnames(scenario), value = TRUE))
  formula <- as.formula(paste(terms, collapse = " + "))
  sdif <- coef(lm(formula, data = scenario))
  cell <- coef(lm(response ~ 0 + time:group, data = data))
  expect_equal(as.numeric(transform %*% sdif), as.numeric(cell))
})

test_that("archetype non-subgroup with nuisance", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  scenario <- brm_archetype_successive_cells(data, prefix_nuisance = "z_")
  transform <- brm_transform_marginal(
    data = scenario,
    formula = brm_formula(scenario)
  )
  expect_equal(
    rownames(transform),
    c(
      "group_1|time_1",
      "group_1|time_2",
      "group_1|time_3",
      "group_2|time_1",
      "group_2|time_2",
      "group_2|time_3"
    )
  )
  expect_equal(
    sort(colnames(transform)),
    sort(
      c(
        "b_x_group_1_time_1",
        "b_x_group_1_time_2",
        "b_x_group_1_time_3",
        "b_x_group_2_time_1",
        "b_x_group_2_time_2",
        "b_x_group_2_time_3",
        "b_z_biomarker1",
        "b_z_biomarker2",
        "b_z_status1_absent",
        "b_z_status2_present"
      )
    )
  )
  expect_equal(
    unname(transform[, seq_len(6)]),
    kronecker(diag(2), diag(3) + lower.tri(diag(3)))
  )
  expect_equal(max(abs(transform[, seq(7, 10)])), 0)
  terms <- c(
    "response ~ 0",
    grep("^x_|^z_", colnames(scenario), value = TRUE)
  )
  formula <- as.formula(paste(terms, collapse = " + "))
  sdif <- coef(lm(formula, data = scenario))
  terms <- c(
    "response ~ 0 + time:group",
    grep("^z_", colnames(scenario), value = TRUE)
  )
  formula <- as.formula(paste(terms, collapse = " + "))
  cell <- coef(lm(formula, data = scenario))
  nuisance <- grep("^z_", names(cell), value = TRUE)
  cell <- cell[c(setdiff(names(cell), nuisance), nuisance)]
  expect_equal(as.numeric(transform %*% sdif), as.numeric(cell)[seq_len(6L)])
})

test_that("archetype subgroup with nuisance, average_within_subgroup = F", {
  data <- brm_simulate_outline(
    n_group = 2,
    n_subgroup = 2,
    n_patient = 100,
    n_time = 3,
    rate_dropout = 0,
    rate_lapse = 0
  ) |>
    dplyr::mutate(response = rnorm(n = dplyr::n())) |>
    brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
    brm_simulate_categorical(
      names = c("status1", "status2"),
      levels = c("present", "absent")
    )
  scenario <- brm_archetype_successive_cells(data, prefix_nuisance = "z_")
  transform <- brm_transform_marginal(
    data = scenario,
    formula = brm_formula(scenario),
    average_within_subgroup = FALSE
  )
  expect_equal(
    rownames(transform),
    c(
      "group_1|subgroup_1|time_1",
      "group_1|subgroup_1|time_2",
      "group_1|subgroup_1|time_3",
      "group_1|subgroup_2|time_1",
      "group_1|subgroup_2|time_2",
      "group_1|subgroup_2|time_3",
      "group_2|subgroup_1|time_1",
      "group_2|subgroup_1|time_2",
      "group_2|subgroup_1|time_3",
      "group_2|subgroup_2|time_1",
      "group_2|subgroup_2|time_2",
      "group_2|subgroup_2|time_3"
    )
  )
  expect_equal(
    sort(colnames(transform)),
    sort(
      c(
        "b_x_group_1_subgroup_1_time_1",
        "b_x_group_1_subgroup_1_time_2",
        "b_x_group_1_subgroup_1_time_3",
        "b_x_group_1_subgroup_2_time_1",
        "b_x_group_1_subgroup_2_time_2",
        "b_x_group_1_subgroup_2_time_3",
        "b_x_group_2_subgroup_1_time_1",
        "b_x_group_2_subgroup_1_time_2",
        "b_x_group_2_subgroup_1_time_3",
        "b_x_group_2_subgroup_2_time_1",
        "b_x_group_2_subgroup_2_time_2",
        "b_x_group_2_subgroup_2_time_3",
        "b_z_biomarker1",
        "b_z_biomarker2",
        "b_z_status1_absent",
        "b_z_status2_present"
      )
    )
  )
  expect_equal(
    unname(transform[, seq_len(12)]),
    kronecker(diag(4), diag(3) + lower.tri(diag(3)))
  )
  expect_equal(max(abs(transform[, seq(13, 16)])), 0)
  terms <- c(
    "response ~ 0",
    grep("^x_|^z_", colnames(scenario), value = TRUE)
  )
  formula <- as.formula(paste(terms, collapse = " + "))
  sdif <- coef(lm(formula, data = scenario))
  terms <- c(
    "response ~ 0 + time:subgroup:group",
    grep("^z_", colnames(scenario), value = TRUE)
  )
  formula <- as.formula(paste(terms, collapse = " + "))
  cell <- coef(lm(formula, data = scenario))
  nuisance <- grep("^z_", names(cell), value = TRUE)
  cell <- cell[c(setdiff(names(cell), nuisance), nuisance)]
  expect_equal(as.numeric(transform %*% sdif), as.numeric(cell)[seq_len(12L)])
})
