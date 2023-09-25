library(crew.cluster)
library(targets)
library(tarchetypes)

tar_option_set(
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  workspace_on_error = TRUE,
  controller = crew_controller_sge(
    workers = 100L,
    seconds_idle = 120,
    seconds_launch = 604800,
    sge_cores = 4L,
    script_lines = paste0("module load R/", getRversion())
  )
)

tar_source()

list(
  tar_target(prior_simple, get_prior_simple()),
  tar_target(prior_complex, get_prior_complex()),
  tar_rep(
    ranks_simple,
    simulate_simple(
      prior = prior_simple,
      chains = 4L,
      warmup = 2000L,
      iter = 4000L
    ),
    batches = 1000,
    reps = 1
  ),
  tar_rep(
    ranks_complex,
    simulate_complex(
      prior = prior_complex,
      chains = 4L,
      warmup = 2000L,
      iter = 4000L
    ),
    batches = 1000,
    reps = 1
  )
)
