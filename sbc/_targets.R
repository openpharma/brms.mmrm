library(crew.aws.batch)
library(targets)
library(tarchetypes)

tar_option_set(
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
#  error = "null",
  workspace_on_error = TRUE,

#controller = crew::crew_controller_local(),

  controller = crew_controller_aws_batch(
    name = "brms-mmrm-sbc",
    workers = 25L,
    seconds_idle = 120,
    seconds_launch = 1800,
    launch_max = 3L,
    processes = 4,
    aws_batch_job_definition = Sys.getenv("JOB_DEFINITION", unset = "job"),
    aws_batch_job_queue = Sys.getenv("JOB_QUEUE", unset = "queue")
  ),

  repository = "aws",
  cue = tar_cue(file = FALSE),
 
  resources = tar_resources(
  aws = tar_resources_aws(
    bucket = Sys.getenv("BUCKET", unset = "bucket"),
    prefix = Sys.getenv("PREFIX", unset = "prefix"),
    region = Sys.getenv("REGION", unset = "region")
  )
 )
)

#tar_option_set(resources = resources)

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
    reps = 10
  ),
  tar_target(simple, ranks_simple),
  tar_rep(
    ranks_complex,
    simulate_complex(
      prior = prior_complex,
      chains = 4L,
      warmup = 2000L,
      iter = 4000L
    ),
    batches = 100,
    reps = 10
  ),
  tar_target(complex, ranks_complex)
)
