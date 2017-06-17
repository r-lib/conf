
#' @importFrom rappdirs user_config_dir

get_package_config_file <- function(pkg) {
  file.path(
    user_config_dir("r-config"),
    pkg,
    "config.yaml"
  )
}
