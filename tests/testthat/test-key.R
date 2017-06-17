
context("Key helper functions")

test_that("get_key_parts", {

  cases <- list(
    list("foo", "foo"),
    list("foo:bar", c("foo", "bar")),
    list("1:2:3:4", c("1", "2", "3", "4"))
  )

  for (c in cases) expect_equal(get_key_parts(c[[1]]), c[[2]], info = c[[1]])
})

test_that("analyze_key", {

  cases <- list(
    list("foo", list(foo = 10), list(common = "foo", rest = character())),
    list("foo", list(bar = 10), list(common = character(), rest = "foo")),
    list("foo:bar", list(foo = 1), list(common = "foo", rest = "bar")),
    list("foo:bar",
         list(foo = list(bar = 1)),
         list(common = c("foo", "bar"), rest = character())),
    list("foo:bar",
         list(foo = list(bar2 = 1)),
         list(common = "foo", rest = "bar")),
    list("foo", list(), list(common = character(), rest = "foo")),
    list("foo:bar",
         list(),
         list(common = character(), rest = c("foo", "bar")))
  )

  for (c in cases) {
    expect_equal(analyze_key(c[[1]], c[[2]]), c[[3]], info = c[[1]])
  }

})
