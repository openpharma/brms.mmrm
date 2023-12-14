library(crew.cluster)
library(targets)
library(tarchetypes)

tar_option_set(
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  workspace_on_error = TRUE,
  controller = crew_controller_sge(
    name = "brms-mmrm-sbc",
    workers = 100L,
    seconds_idle = 120,
    seconds_launch = 1800,
    launch_max = 3L,
    script_lines = "module load R/4.2.2",
    sge_cores = 4L
  )
  # controller = crew_controller_aws_batch(
  #   name = "brms-mmrm-sbc",
  #   workers = 50L,
  #   seconds_idle = 120,
  #   seconds_launch = 1800,
  #   launch_max = 3L,
  #   processes = 4,
  #   aws_batch_job_definition = Sys.getenv("JOB_DEFINITION", unset = "job"),
  #   aws_batch_job_queue = Sys.getenv("JOB_QUEUE", unset = "queue")
  # ),
  # repository = "aws",
  # resources = tar_resources(
  #   aws = tar_resources_aws(
  #     bucket = Sys.getenv("BUCKET", unset = "bucket"),
  #     prefix = Sys.getenv("PREFIX", unset = "prefix"),
  #     region = Sys.getenv("REGION", unset = "region")
  #   )
  # )
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
    batches = 100,
    reps = 1 #10
  ),
  tar_rep(
    ranks_complex,
    simulate_complex(
      prior = prior_complex,
      chains = 4L,
      warmup = 2000L,
      iter = 4000L
    ),
    batches = 100,
    reps = 1 #10
  ),
  tar_target(complex, bind_ranks(ranks_complex)),
  tar_target(simple, bind_ranks(ranks_simple)),
  tar_file(
    file_prior_simple,
    prior_simple |>
      dplyr::select(prior, class, coef, dpar) |>
      save_fst("../vignettes/sbc/prior_simple.fst"),
    deployment = "main",
    repository = "local"
  ),
  tar_file(
    file_prior_complex,
    prior_complex |>
      dplyr::select(prior, class, coef, dpar) |>
      save_fst("../vignettes/sbc/prior_complex.fst"),
    deployment = "main",
    repository = "local"
  ),
  tar_file(
    file_simple,
    save_fst(simple, "../vignettes/sbc/simple.fst"),
    deployment = "main",
    repository = "local"
  ),
  tar_file(
    file_complex,
    save_fst(complex, "../vignettes/sbc/complex.fst"),
    deployment = "main",
    repository = "local"
  )
)
