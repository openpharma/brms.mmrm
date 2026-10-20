save_csv <- function(object, path) {
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path))
  }
  utils::write.csv(object, path, row.names = FALSE)
  path
}
