
#' Persistent Package Configuration
#'
#' Store the configuration of your package in the user's platform dependent
#' config file directory. The configuration persists across R sessions,
#' and can also be edited manually. Configuration files are YAML files.
#'
#' @docType package
#' @name conf
#' @importFrom assertthat assert_that
NULL

#' @export
#' @importFrom R6 R6Class

conf <- R6Class(
  "conf",

  public = list(
    initialize = function(package = NULL, file = NULL, lock = FALSE)
      cf_init(self, private, package, file, lock),
    get = function(key = NULL, default = NULL)
      cf_get(self, private, key, default),
    set = function(key, value)
      cf_set(self, private, key, value),
    delete = function(key)
      cf_delete(self, private, key),
    has = function(key)
      cf_has(self, private, key),
    get_keys = function(at = NULL)
      cf_get_keys(self, private, at),
    clear = function()
      cf_clear(self, private),
    save = function(path = NULL, unlock = TRUE)
      cf_save(self, private, path, unlock),
    lock = function(exclusive = TRUE, timeout = Inf)
      cf_lock(self, private, exclusive, timeout),
    unlock = function()
      cf_unlock(self, private),
    get_path = function()
      cf_get_path(self, private),
    print = function(...)
      cf_print(self, private, ...),
    format = function()
      cf_format(self, private)
  ),

  private = list(
    data = NULL,                        # The current config
    path = NULL,                        # Path to actual file
    filelock = NULL                     # File lock (if locked)
  )
)

#' @importFrom yaml yaml.load_file

cf_init <- function(self, private, package, file, lock) {
  assert_that(is_string_or_null(package))
  assert_that(is_string_or_null(file))
  assert_that(is_flag(lock))
  if (!is.null(package) && !is.null(file)) {
    stop("Only at most one of ", sQuote(package), " and ", sQuote(file),
         "can be given")
  }

  private$path <- if (!is.null(file)) {
    file
  } else {
    package <- package %||% environmentName(topenv(parent.frame()))
    get_package_config_file(package)
  }

  if (lock) self$lock()

  if (file.exists(private$path)) {
    private$data <- yaml.load_file(private$path)
  } else {
    private$data <- list()
  }

  invisible(self)
}

cf_get <- function(self, private, key, default) {
  assert_that(is_string_or_null(key))
  if (is.null(key)) return(private$data)

  akey <- analyze_key(key, private$data)
  if (length(akey$rest)) {
    default
  } else if (!length(akey$common)) {
    private$data
  } else {
    private$data[[akey$common]]
  }
}

#' TODO: how does this work?
#'
#' @param self Self.
#' @param private Private self.
#' @param key Key to set.
#' @param value Value to set.
#'
#' @keywords internal
#' @importFrom utils head tail

cf_set <- function(self, private, key, value) {
  assert_that(is_string(key))
  akey <- analyze_key(key, private$data)

  ## If the element we are adding to is not a list, then we coerce it
  ## to a list. This is in case we want to add more members to it.
  if (length(akey$common) && !is.list(private$data[[akey$common]])) {
    private$data[[akey$common]] <- as.list(private$data[[akey$common]])
  }

  private$data[[c(akey$common, head(akey$rest, 1))]] <-
    deep_list(tail(akey$rest, -1), value)

  invisible(self)
}

cf_delete <- function(self, private, key) {
  assert_that(is_string(key))
  key_parts <- get_key_parts(key)
  private$data[[key_parts]] <- NULL
  invisible(self)
}

cf_has <- function(self, private, key) {
  assert_that(is_string(key))
  akey <- analyze_key(key, private$data)
  length(akey$rest) == 0
}

cf_get_keys <- function(self, private, at) {
  assert_that(is_string_or_null(at))
  if (is.null(at)) {
    as.character(names(private$data))
  } else {
    at_parts <- get_key_parts(at)
    as.character(names(private$data[[at_parts]]))
  }
}

cf_clear <- function(self, private) {
  private$data <- list()
  invisible(self)
}

cf_save <- function(self, private, path, unlock) {
  assert_that(is_string_or_null(path))
  assert_that(is_flag(unlock))

  path <- path %||% private$path
  cat(self$format(), file = path)

  if (unlock) self$unlock()

  invisible(self)
}

#' @importFrom filelock lock unlock

cf_lock <- function(self, private, exclusive, timeout) {
  lock_file <- paste0(private$path, ".lock")
  private$filelock <- lock(
    lock_file,
    exclusive = exclusive,
    timeout = timeout
  )
  invisible(self)
}

cf_unlock <- function(self, private) {
  if (!is.null(private$filelock)) unlock(private$filelock)
  invisible(self)
}

cf_get_path <- function(self, private) {
  private$path
}

cf_print <- function(self, private, ...) {
  cat(self$format())
  invisible(self)
}

#' @importFrom yaml as.yaml

cf_format <- function(self, private) {
  as.yaml(private$data)
}
