library(namedCapture)
library(testthat)
context("errors")

subject.vec <- c("foo", "bar")

test_that("no capture groups is an error", {
  expect_error({
    str_match_named(subject.vec, "o")
  }, "(?<name>subpattern)", fixed=TRUE)
})

test_that("no named capture groups is an error", {
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
