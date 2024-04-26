`%||%` <- function(x, y) {
  if (length(x) <= 0L) {
    y
  } else {
    x
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%||nzchar%` <- function(x, y) {
  if (nzchar(x)) {
    x
  } else {
    y
  }
}

if_any <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}
