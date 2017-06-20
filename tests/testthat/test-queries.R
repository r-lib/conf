
context("Queries")

test_that("init", {
  expect_error(
    conf$new(package = "foobar", file = "foobar2"),
    "Only at most one"
  )

  expect_match(
    conf$new(package = "foobar")$get_path(),
    "foobar",
    fixed = TRUE
  )
})

test_that("get", {

  tmp <- make_temp_conf(list(foo = 42, bar = 100))
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_equal(cf$get("foo"), 42)
  expect_equal(cf$get("bar"), 100)
  expect_equal(cf$get("x"), NULL)
  expect_equal(cf$get("foo:bar"), NULL)
  expect_equal(cf$get("xx:xx"), NULL)
  expect_equal(cf$get(""), list(foo = 42, bar = 100))
})

test_that("more get", {

  tmp <- make_temp_conf(
    list(foo = list(bar = list(foobar = 1:10)),
         xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_equal(cf$get("foo"), list(bar = list(foobar = 1:10)))
  expect_equal(cf$get("xxx"), "hello")
  expect_equal(cf$get("foo:bar"), list(foobar = 1:10))
  expect_equal(cf$get("foo:bar:foobar"), 1:10)

  expect_equal(cf$get("foo:xxx"), NULL)
})

test_that("set", {

  tmp <- make_temp_conf(
    list(foo = list(bar = list(foobar = 1:10)),
         xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  cf$set("xxx", "bye")
  expect_equal(cf$get("xxx"), "bye")

  cf$set("new", "hello again")
  expect_equal(cf$get("new"), "hello again")

  cf$set("new2:huh", "yes?")
  expect_equal(cf$get("new2"), list(huh = "yes?"))

  tmp2 <- make_temp_conf(list(a = list(b = 42)))
  on.exit(unlink(tmp2), add = TRUE)
  cf0 <- conf$new(file = tmp2)

  cf <- cf0$clone()
  cf$set("a:b", 1:10)
  expect_equal(cf$get("a"), list(b = 1:10))

  cf <- cf0$clone()
  cf$set("a:b:c", 1:10)
  expect_equal(cf$get("a"), list(b = list(42, c = 1:10)))

  cf <- cf0$clone()
  cf$set("a:b:c:d", 1:10)
  expect_equal(cf$get("a"), list(b = list(42, c = list(d = 1:10))))

  tmp3 <- make_temp_conf(list(a = 42))
  on.exit(unlink(tmp3), add = TRUE)
  cf0 <- conf$new(file = tmp3)

  cf <- cf0$clone()
  cf$set("a", 1:10)
  expect_equal(cf$get(), list(a = 1:10))

  cf <- cf0$clone()
  cf$set("x", 1:10)
  expect_equal(cf$get(), list(a = 42, x = 1:10))

  tmp4 <- make_temp_conf(list())
  on.exit(unlink(tmp4), add = TRUE)
  cf <- conf$new(file = tmp4)
  cf$set("a", 1:10)
  expect_equal(cf$get(), list(a = 1:10))
})

test_that("delete", {

  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf0 <- conf$new(file = tmp)

  cf <- cf0$clone()
  cf$delete("yyy")
  expect_equal(cf$get(), data)

  cf <- cf0$clone()
  cf$delete("xxx")
  expect_equal(
    cf$get(),
    list(foo = list(bar = list(foobar = 1:10, baz = "yeah")))
  )

  cf <- cf0$clone()
  cf$delete("foo:bar:foobar")
  expect_equal(
    cf$get(),
    list(foo = list(bar = list(baz = "yeah")), xxx = "hello")
  )

  cf <- cf0$clone()
  cf$delete("foo:bar:xxx")
  expect_equal(cf$get(), data)

  cf <- cf0$clone()
  cf$delete("foo:bar")
  expect_equal(cf$get(), list(foo = empty_named_list(), xxx = "hello"))
})

test_that("has", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_true(cf$has("foo"))
  expect_true(cf$has("foo:bar"))
  expect_true(cf$has("foo:bar:foobar"))
  expect_true(cf$has("foo:bar:baz"))
  expect_true(cf$has("xxx"))

  expect_false(cf$has("yyy"))
  expect_false(cf$has("xxxx"))
  expect_false(cf$has("xxx:bar"))
  expect_false(cf$has("foo:bar:xxx"))
})

test_that("get_keys", {

  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_equal(cf$get_keys(), c("foo", "xxx"))
  expect_equal(cf$get_keys(at = "foo"), "bar")
  expect_equal(cf$get_keys(at = "foo:bar"), c("foobar", "baz"))
  expect_equal(cf$get_keys(at = "xxx"), character())
  expect_equal(cf$get_keys(at = "foo:bar:foobar"), character())
})

test_that("clear", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  cf$clear()
  expect_equal(cf$get(), list())

  cf$clear()
  expect_equal(cf$get(), list())
})

test_that("save", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  cf$set("xxx", "bye")
  cf$save()

  cf <- conf$new(file = tmp)
  expect_equal(
    cf$get(),
    list(foo = list(bar = list(foobar = 1:10, baz = "yeah")), xxx = "bye")
  )
})

test_that("lock, unlock", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp, lock = TRUE)

  lock_file <- paste0(tmp, ".lock")

  x <- callr::r_safe(
    function(f) filelock::lock(f, timeout = 0),
    args = list(f = lock_file)
  )
  expect_null(x)

  cf$unlock()
  x <- callr::r_safe(
    function(f) filelock::lock(f, timeout = 0),
    args = list(f = lock_file)
  )
  ## By the time we get it, it will be unlocked already
  expect_equal(class(x), "filelock_lock")

  cf <- conf$new(file = tmp, lock = FALSE)
  x <- callr::r_safe(
    function(f) filelock::lock(f, timeout = 0),
    args = list(f = lock_file)
  )
  ## By the time we get it, it will be unlocked already
  expect_equal(class(x), "filelock_lock")
})

test_that("get_path", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_equal(cf$get_path(), tmp)
})

test_that("print", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  p <- paste0(paste(capture.output(print(cf)), collapse = "\n"), "\n")
  expect_equal(p, cf$format(), fixed = TRUE)
})

test_that("format", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  expect_equal(cf$format(), yaml::as.yaml(cf$get()))
})

test_that("reload", {
  tmp <- make_temp_conf(
    data <- list(foo = list(bar = list(foobar = 1:10, baz = "yeah")),
                 xxx = "hello")
  )
  on.exit(unlink(tmp), add = TRUE)
  cf <- conf$new(file = tmp)

  cf$set("xxx", "bye")
  expect_equal(cf$get("xxx"), "bye")

  cf$reload()
  expect_equal(cf$get("xxx"), "hello")
})

test_that("error if cannot lock", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  lock <- get_lock_name(tmp)
  bg <- callr::r_bg(
    function(p) print(filelock::lock(p, exclusive = TRUE)),
    stdout = "|", stderr = "|", args = list(lock)
  )
  bg$poll_io(2000)
  on.exit(bg$kill(), add = TRUE)

  expect_error(
    conf$new(file = tmp, lock = TRUE,
             lock_exclusive = TRUE, lock_timeout = 0 ),
    class = "file_locking_error"
  )

  bg$kill()
})
