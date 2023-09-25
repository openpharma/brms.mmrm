random_lkj <- function() {
  sprintf("lkj(%s)", runif(n = 1L, min = 1, max = 1.5))
}

random_t <- function() {
  sprintf(
    "student_t(%s, %s, %s)",
    runif(n = 1L, min = 3, max = 4),
    runif(n = 1L, min = -0.25, max = 0.25),
    runif(n = 1L, min = 1, max = 2)
  )
}
