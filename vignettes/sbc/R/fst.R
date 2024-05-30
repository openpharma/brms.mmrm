save_fst <- function(object, path) {
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path))
  }
  fst::write_fst(object, path)
  path
}
