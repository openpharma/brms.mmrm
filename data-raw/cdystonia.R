Hmisc::getHdata(cdystonia)
cdystonia <- cdystonia |>
  dplyr::filter(treat %in% c("10000U", "Placebo")) |>
  droplevels() |>
  dplyr::mutate_all(
    ~ {
      attr(., "label") <- NULL
      .
    }
  ) |>
  dplyr::mutate(
    sex = as.character(sex),
    week = paste0("w", sprintf("%02d", week)),
    id = paste0(site, "-", sprintf("%02d", id))
  ) |>
  dplyr::select(id, age, sex, treat, week, twstrs) |>
  dplyr::arrange(id, week) |>
  tibble::as_tibble()
usethis::use_data(cdystonia, overwrite = TRUE)
