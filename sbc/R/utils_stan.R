random_lkj_text <- function() {
  sprintf("lkj(%s)", runif(n = 1L, min = 1, max = 1.5))
}

lkj <- function(shape) {
  shape
}

random_normal_text_b <- function() {
  sprintf(
    "normal(%s, %s)",
    runif(n = 1L, min = -0.25, max = 0.25),
    runif(n = 1L, min = 0.25, max = 3)
  )
}

random_normal_text_b_sigma <- function() {
  sprintf(
    "normal(%s, %s)",
    runif(n = 1L, min = -1, max = 1),
    runif(n = 1L, min = 0.25, max = 1)
  )
}

normal <- function(mean, sd) {
  stats::rnorm(n = 1L, mean = mean, sd = sd)
}
