unname_df <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- unname(x[[i]])
  }
  x
}

zero_pad_integers <- function(x) {
  sprintf(paste0("%0", max(nchar(as.character(x))), "d"), as.integer(x))
}
