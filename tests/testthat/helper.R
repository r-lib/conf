
make_temp_conf <- function(obj) {
  cat(
    yaml::as.yaml(obj),
    file = tmp <- tempfile()
  )
  tmp
}
