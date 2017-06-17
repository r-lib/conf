
get_key_parts <- function(key) {
  if (key == "") {
    character()
  } else {
    strsplit(key, ":", fixed = TRUE)[[1]]
  }
}

#' Analyze a key with respect to some data
#'
#' @param key key
#' @param data data
#' @return List with elements:
#'   * `common`: character vector, the common part of the key and the data.
#'   * `rest`: the rest of the *key*.
#'
#' @keywords internal

analyze_key <- function(key, data) {
  parts <- get_key_parts(key)
  common <- character()
  for (p in parts) {
    if (p %in% names(data)) {
      common <- c(common, p)
      data <- data[[p]]
    } else {
      break
    }
  }

  list(
    common = common,
    rest = if (length(common)) tail(parts, - length(common)) else parts
  )
}
