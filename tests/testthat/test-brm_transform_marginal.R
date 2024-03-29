test_that("brm_transform_marginal(), change, non-subgroup", {
  set.seed(0L)
  suppressPackageStartupMessages(skip_if_not_installed("mmrm"))
  data(fev_data, package = "mmrm")
  raw_data <- fev_data
  raw_data$FEV1[is.na(raw_data$FEV1)] <- rnorm(n = sum(is.na(raw_data$FEV1)))
  raw_data$FEV1_CHG <- raw_data$FEV1 - raw_data$FEV1_BL
  grid <- dplyr::distinct(raw_data, ARMCD, AVISIT)
  grid <- dplyr::arrange(grid, ARMCD, AVISIT)
  row_names <- paste(grid$ARMCD, grid$AVISIT, sep = "|")
  data <- brm_data(
    data = raw_data,
    outcome = "FEV1_CHG",
    role = "change",
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
  transform <- brm_transform_marginal(
    data = data,
    formula = formula,
    prefix = ""
  )
  model_matrix <- brms::make_standata(formula = formula, data = data)$X
  expect_equal(colnames(transform), colnames(model_matrix))
  expect_equal(rownames(transform), row_names)
  skip_if_not_installed("emmeans")
  old_sep <- emmeans::get_emm_option("sep")
  on.exit(emmeans::emm_options(sep = old_sep))
  emmeans::emm_options(sep = "|")
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
  exp <- emmeans(
    object = model,
    specs = ~ARMCD:AVISIT,
    weights = "proportional",
    nuisance = c("RACE", "SEX", "WEIGHT")
  )
  exp <- as.data.frame(exp)
  exp <- setNames(exp$emmean, paste0(exp$ARMCD, "|", exp$AVISIT))
  exp <- exp[names(out)]
  expect_equal(out, exp)
})
