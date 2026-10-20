options(brms.backend = "cmdstanr")
sources <- list.files(pattern = "\\.Rmd\\.source$")
outputs <- sub("\\.source$", "", sources)
for (i in seq_along(sources)) {
  message(sources[i])
  knitr::knit(input = sources[i], output = outputs[i], quiet = TRUE)
}
