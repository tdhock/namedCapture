library(namedCapture)
library(testthat)
context("errors")

subject.vec <- c("foo", "bar")

test_that("no capture groups is an error", {
  expect_error({
    str_match_named(subject.vec, "o")
  }, "(?<name>subpattern)", fixed=TRUE)
})

test_that("any capture group without a name is an error", {
  expect_error({
    str_match_named(subject.vec, "(o)(?<name>o)")
  }, "(?<name>subpattern)", fixed=TRUE)
})

test_that("NA pattern is an error", {
  expect_error({
    str_match_named(subject.vec, NA_character_)
  })
})

test_that("multiple patterns is an error", {
  expect_error({
    str_match_named(subject.vec, c("(?<name>.)", "(?<name>.)"))
  })
})

test_that("subject of length 0 is an error", {
  expect_error({
    str_match_named(character(), "(?<name>.)")
  }, "0 < length(subject.vec) is not TRUE", fixed=TRUE)
})

test_that("still works if NA first and only name", {
  subject <- c(missing=NA, nomatch="", match="foobar")
  result.list <- str_match_all_named(subject, "(?<name>foo)")
  expected.list <- list(
    missing=matrix(character(), 0, 0),
    nomatch=matrix(character(), 0, 0),
    match=matrix(character(), 1, 0, dimnames=list("foo")))
  expect_identical(result.list, expected.list)
})

pattern.not.greedy <- paste0(
  "(?<chrom>chr.*?)",
  ":",
  "(?<chromStart>.*?)",
  "-",
  "(?<chromEnd>[0-9,]*)")

test_that("informative error when converter is not function", {
  expect_error({
    str_match_named("chr2:300-400", pattern.not.greedy, list(chromStart="foo"))
  }, "type.list must be list(group.name=function(character.vector)any.vector)",
               fixed=TRUE)
})

test_that("informative error when converter fun has zero args", {
  expect_error({
    str_match_named("chr2:300-400", pattern.not.greedy, list(
      chromStart=function()y))
  }, "type.list must be list(group.name=function(character.vector)any.vector)",
               fixed=TRUE)
})

test_that("informative error when converter returns wrong length", {
  expect_error({
    str_match_named(
      c("chr2:300-400", "chr2:300-400"),
      pattern.not.greedy, list(
      chromStart=function(x)"foo"))
  }, "chromStart type.list function returned vector of length 1 but expected length 2")
})

test_that("informative error when converter returns non-atomic", {
  expect_error({
    str_match_named(
      c("chr2:300-400", "chr2:300-400"),
      pattern.not.greedy, list(
      chromStart=function(x)list(foo=200)))
  }, "chromStart type.list function must return atomic vector")
})
