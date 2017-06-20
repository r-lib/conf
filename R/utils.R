
`%||%` <- function(l, r) if (is.null(l)) r else l

deep_list <- function(names, value) {
  Reduce(
    function(l, n) structure(list(l), names = n),
    rev(names),
    value
  )
}

empty_named_list <- function() {
  structure(list(), names = character())
}

get_lock_name <- function(path) {
  paste0(path, ".lock")
}
