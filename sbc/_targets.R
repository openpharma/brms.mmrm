library(targets)
library(tarchetypes)

tar_option_set(
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  workspace_on_error = TRUE
)

tar_source()

envir <- new.env(parent = baseenv())
eval(parse(text = readLines("R/scenarios.R")), envir = envir)
scenarios <- list(name = names(envir))
scenarios$scenario <- rlang::syms(scenarios$name)

list(
  tar_map(
    values = scenarios,
    names = tidyselect::any_of("name"),
    tar_target(prior, setup_prior(scenario)),
    tar_rep(
      name = ranks,
      run_simulation(
        setup_function = scenario,
        prior = prior,
        chains = 1, #4L,
        warmup = 10, #2000L,
        iter = 20 #4000L
      ),
      batches = 1,
      reps = 1
    ),
    tar_target(
      file,
      save_fst(ranks, sprintf("../vignettes/sbc/%s.fst", name)),
      deployment = "main"
    )
  )
)
