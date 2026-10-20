options(brms.backend = "cmdstanr")
sources <- list.files(pattern = "\\.Rmd\\.source$")
outputs <- sub("\\.source$", "", sources)
for (i in seq_along(sources)) {
  message(sources[i])
  knitr::knit(input = sources[i], output = outputs[i], quiet = TRUE)
  # knit() evaluates inline R in the YAML header, hard-coding local paths.
  source_lines <- readLines(sources[i])
  output_lines <- readLines(outputs[i])
  for (field in c("bibliography", "csl")) {
    pattern <- sprintf("^%s:", field)
    output_lines[grep(pattern, output_lines)[1L]] <-
      source_lines[grep(pattern, source_lines)[1L]]
  }
  writeLines(output_lines, outputs[i])
}
