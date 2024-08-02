library(targets)
library(tarchetypes)

tar_option_set(
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  workspace_on_error = TRUE,
  controller = crew.cluster::crew_controller_sge(
    workers = 250L,
    seconds_idle = 120,
    seconds_interval = 10,
    seconds_timeout = 120,
    log_resources = "memory.txt",
    sge_cores = 3L,
    sge_memory_gigabytes_required = 32,
    sge_log_output = "logs/",
    script_lines = file.path("module load R", getRversion())
  )
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
        scenario = scenario,
        prior = prior,
        chains = 3L,
        warmup = 4000L,
        iter = 8000L
      ),
      batches = 1000,
      reps = 1
    ),
    tar_target(
      results,
      save_fst(ranks, sprintf("results/%s.fst", name)),
      deployment = "main"
    )
  )
)
