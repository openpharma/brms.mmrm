assert <- function(
  value = NULL,
  ...,
  message = NULL,
  envir = parent.frame()
) {
  expr <- match.call(expand.dots = FALSE)$...
  if (!length(expr)) {
    expr <- list(quote(.))
  }
  conditions <- lapply(
    expr,
    function(expr) all(eval(expr, envir = list(. = value), enclos = envir))
  )
  if (!all(unlist(conditions))) {
    chr_expr <- lapply(expr, function(x) sprintf("all(%s)", deparse(x)))
    chr_expr <- paste(unlist(chr_expr), collapse = " && ")
    chr_value <- deparse(substitute(value))
    out <- sprintf("%s is not true on . = %s", chr_expr, chr_value)
    brm_error(message %|||% out)
  }
}

assert_col <- function(value, data, message = NULL) {
  message <- message %|||% paste(
    paste(value, collapse = ", "),
    "must be column name(s) of",
    deparse(substitute(data))
  )
  assert(
    all(value %in% colnames(data)),
    message = message
  )
}


assert_chr_vec <- function(value, message = NULL) {
  assert(
    value,
    is.character(.),
    !anyNA(.),
    nzchar(.),
    message = message
  )
}

assert_chr <- function(value, message = NULL) {
  assert_chr_vec(value, message = message)
  assert(value, length(.) == 1L, message = message)
}

assert_lgl <- function(value, message = NULL) {
  assert(value, isTRUE(.) || isFALSE(.), message = message)
}

assert_num <- function(value, message = NULL) {
  assert(value, is.numeric(.), !anyNA(.), length(.) == 1L, message = message)
}

assert_pos <- function(value, message = NULL) {
  assert_num(value, message = message)
  assert(value, . > 0, message = message)
}

brm_error <- function(message) {
  rlang::abort(message = message, class = "brm_error", .frame = emptyenv())
}

brm_warn <- function(message) {
  rlang::warn(message = message, class = "brm_warn")
}
