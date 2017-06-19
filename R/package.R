
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

#' Class for Package Configuration
#'
#' @section Usage:
#' ```
#' cf <- conf$new(package = NULL, file = NULL, lock = NULL)
#'
#' cf$get(key = NULL, default = NULL)
#' cf$set(key, value)
#' cf$delete(key)
#' cf$has(key)
#' cf$get_keys(at = NULL)
#' cf$clear()
#' cf$save(path = NULL, unlock = TRUE)
#' cf$reload()
#' cf$lock(exclusive = TRUE, timeout = Inf)
#' cf$unlock()
#' cf$get_path()
#' cf$print(...)
#' cf$format()
#' ```
#'
#' @section Arguments:
#' * `package`: Package to configure. This cannot be specified together
#'   with the `file` argument. The location of the configuration file will
#'   be in the platform specific configuration directory, see
#'   [rappdirs::user_config_dir()]. Alternatively the `file` argument can be
#'   used to specify the configuration file directly.
#' * `file`: Path to the configuration file. This cannot be specified
#'   together with the `package` argument.
#' * `lock`: Logical flag, whether to lock the configuration file before
#'   opening it. Locking makes sure that no other R process (that uses the
#'   `conf` package) will have read or write access to it. If you lock the
#'   file, then it is a good idea to unlock it as soon as you don't need it.
#'   See also the `lock()` and `unlock()` methods, and the `unlock` argument
#'   of the `save()` method.
#' * `key`: The key of the record to read or write or delete, etc.
#'   It is a string scalar. The key can be nested, use `:` to
#'   separate the components. I.e. the key `foo:bar:foobar` denotes the
#'   `foobar` element within the `bar` element within the `foo` element.
#' * `default`: The value to return if the specified key does not exist.
#' * `value`: New value of the key. Any R object that can be converted to
#'    YAML using the [yaml::as.yaml()] function.
#' * `at`: A key, within which the names of the subkeys are listed.
#' * `path`: Path to save the file to. Defaults to `NULL` which means the
#'    path that was used when the object was created.
#' * `unlock`: Logical flag. Whether to unlock the file, after saving.
#'   If the file was not locked, then this flag does nothing.
#' * `exclusive`: Whether to create an exclusive lock. See
#'   [filelock::lock()]].
#' * `timeout`: Timeout to acquire the lock. See [filelock::lock()].
#' * `...`: Extra arguments, currently ignored. This is ignored to match
#'   the signature of `base::print()`.
#'
#' @section Details:
#'
#' `conf$new()` creates a new configuration object. Specify either the
#' name of the package this file belongs to, or the path of the config
#' file directly. You can also lock the file, which is a good idea if you
#' already know that you'll modify it.
#'
#' `cf$get()` queries a record in the config file. By default it returns
#' all configuration data. The YAML data will be converted to an R object
#' via the [yaml::yaml.load_file()] function. Note that this function
#' simplifies lists to vectors, if all elements are of the same type.
#'
#' `cf$set()` sets a record to a new value in the config file. The key
#' of the record can be hierarchical, and missing levels are automatically
#' created. I.e. if the configation file has a `foo` entry, but no
#' `foo:bar` entry, and key is `foo:bar:foobar`, then `bar` and below that
#' `foobar` will be created, within the existing `foo` entry. Note that
#' `cf$set()` only modifies the configuration object, and not the file on
#' disk. Call the `save()` method to modify the file. It is a good idea to
#' lock the file before opening it, if you already know that you would
#' modify it.
#'
#' `cf$delete()` deletes a record. Specifying a non-existant record does
#' nothing. It is allowed to specify a hierarchical key.
#'
#' `cf$has()` decides whether a config file has the given key.
#'
#' `cf$get_keys()` lists the names of the keys within the given key.
#'
#' `cf$clear()` removes everything from the configuration file.
#'
#' `cf$save()` writes the configuration file to the disk. By default
#' the file is unlocked after saving, if it was locked before.
#'
#' `cf$reload()` reloads the configuration.
#'
#' `cf$lock()` locks the configuration file, so other R processes will
#' have no access to it. It is good practice to lock the file immediately
#' when opening it, via the `lock` argument of the initialization. If you
#' use the `lock()` method to lock it later, consider reloading the
#' configation *after* locking it, in case another process has changed
#' it between the previous reading the locking.
#'
#' `cf$unlock()` unlocks a configuration file.
#'
#' `cf$get_path()` returns the path to the configuration file that was
#' specified when creating the `conf` object.
#'
#' `cf$print()` prints the configuration to the screen, in YAML format.
#'
#' `cf$format()` formats and returns the printout of the configuration,
#' in YAML format.
#'
#' @name conf
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
    reload = function()
      cf_reload(self, private),
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

#' Set a configuration item
#'
#' First we check common part in the key and the data.
#'
#' ```
#' data  key     common rest car(rest) cdr(rest) ops
#' ----- ---     ------ ---- --------- --------- --------------------
#' a:b:0 a:b     a,b                             [[c(int, c)]] <- value
#' a:b:0 a:b:c   a,b    c    c                   [[c(int, c)]] <- value
#' a:b:0 a:b:c:d a,b    c,d  c         d         [[c(int, c)]] <- d:value
#' a:0   a       a                               [[c(int, c)]] <- value
#' a:0   x              x    x                   [[c(int, c)]] <- value
#'       a              a    a                   [[c(int, a)]] <- value
#' ```
#'
#' Assuming that the tail of the data is a list, all we need to do is
#'
#' ```
#' data[[c(common, head(rest, 1))]] <- deep_list(tail(rest, -1), value)
#' ```
#'
#' where `deep_list` creates a deeply nested list. If `rest` is empty,
#' then we are replacing an existing element in the data list. Otherwise
#' we add a new element to the list at the specified level.
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

cf_reload <- function(self, private) {
  private$data <- yaml.load_file(private$path)
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

  if (is.null(private$filelock)) {
    stop("Cannot lock config file ", sQuote(private$path))
  }

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
