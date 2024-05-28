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
  ),
  tar_target(complex, ranks_complex),
  tar_target(simple, ranks_simple),
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
