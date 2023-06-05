unname_df <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- unname(x[[i]])
  }
  x
}
