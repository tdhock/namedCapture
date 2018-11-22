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
  }, "pattern should be a character scalar (not missing/NA)", fixed=TRUE)
})

test_that("factor pattern is an error", {
  expect_error({
    str_match_named(subject.vec, factor("(?<regex>foo)"))
  }, "pattern should be a character scalar")
})

test_that("multiple patterns is an error", {
  expect_error({
    str_match_named(subject.vec, c("(?<name>.)", "(?<name>.)"))
  }, "pattern should be a character scalar")
})

test_that("subject of length 0 is an error", {
  expect_error({
    str_match_named(character(), "(?<name>.)")
  }, "subject.vec should be a character vector with length>0", fixed=TRUE)
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

test_that("informative error for non-unique chromStart int names", {
  expect_error({
    str_match_named(
      c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
      "(?<chrom>[^:]+):(?<name>[0-9]+)",
      list(name=as.integer))
  }, "capture group named 'name' must be unique")
})

test_that("informative error for non-unique chromStart names", {
  expect_error({
    str_match_named(
      c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
      "(?<chrom>[^:]+):(?<name>[0-9]+)")
  }, "capture group named 'name' must be unique")
})

test_that("informative error for non-unique chrom names", {
  expect_error({
    str_match_named(
      c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
      "(?<name>[^:]+):(?<chromStart>[0-9]+)")
  }, "capture group named 'name' must be unique")
})

test_that("error for name group with missing subject", {
  expect_error({
    str_match_named(
      c("chr1:20-40", NA, "chr2:300-400"),
      "(?<name>[^:]+):(?<chromStart>[0-9]+)")
  }, "the 'name' group should not be missing/NA")
})

test_that("error for name group with two missing subjects", {
  expect_error({
    str_match_named(
      c("chr1:20-40", NA, NA, "chr2:300-400"),
      "(?<name>[^:]+):(?<chromStart>[0-9]+)")
  }, "the 'name' group should not be missing/NA")
})

test_that("error when no match with name group", {
  expect_error({
    str_match_named(
      c("chr1:20-40", "foobar", "chr2:300-400"),
      "(?<name>[^:]+):(?<chromStart>[0-9]+)")
  }, "the 'name' group should not be missing/NA")
})

name.value.vec <- c(
  "  sampleType=monocyte   assayType=H3K27me3    cost=5",
  "sampleType=monocyte assayType=H3K27ac",
  " assayType=Myeloidcell cost=30.5  assayType=H3K4me3")
name.value.pattern <- paste0(
  "(?<name>[^ ]+?)",
  "=",
  "(?<value>[^ ]+)")
test_that("error for non-unique name in match_all", {
  expect_error({
    str_match_all_named(name.value.vec, name.value.pattern)
  }, "capture group named 'name' must be unique")
})

