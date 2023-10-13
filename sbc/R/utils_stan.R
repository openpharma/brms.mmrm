random_lkj_text <- function() {
  sprintf("lkj(%s)", runif(n = 1L, min = 1, max = 1.5))
}

lkj <- function(shape) {
  shape
}

random_t_text <- function() {
  sprintf(
    "student_t(%s, %s, %s)",
    runif(n = 1L, min = 3, max = 4),
    runif(n = 1L, min = -0.25, max = 0.25),
    runif(n = 1L, min = 1, max = 2)
  )
}

student_t <- function(df, center, scale) {
  stats::rt(n = 1L, df = df, ncp = 0) * scale + center
}
