Hmisc::getHdata(cdystonia)
cdystonia <- cdystonia |>
  dplyr::filter(treat %in% c("10000U", "Placebo")) |>
  droplevels() |>
  dplyr::mutate_all(~ {
    attr(., "label") <- NULL
    .
  }) |>
  dplyr::mutate(
    week = paste0("w", sprintf("%02d", week)),
    id = paste0(site, "-", sprintf("%02d", id))
  ) |>
  dplyr::select(-site) |>
  tibble::as_tibble() |>
  brms.mmrm::brm_data(
    outcome = "twstrs",
    role = "response",
    group = "treat",
    time = "week",
    patient = "id",
    covariates = c("age", "sex"),
    level_control = "Placebo",
    level_baseline = "w00"
  ) |>
  brms.mmrm::brm_data_change(name_change = "chg") |>
  dplyr::filter(!is.na(chg)) |>
  dplyr::select(id, sex, age, baseline, treat, week, chg) |>
  dplyr::arrange(id, week)
usethis::use_data(cdystonia, overwrite = TRUE)
