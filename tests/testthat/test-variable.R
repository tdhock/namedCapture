library(testthat)
library(namedCapture)
context("variable args syntax")

subject <- c(
  ten="chr10:213,054,000-213,055,000",
  chrNA="chrNA:111,000-222,000",
  no.match="foo bar",
  missing=NA,
  two="chr1:110-111 chr2:220-222")

test_that("str_match_variable returns character matrix", {
  computed <- str_match_variable(
    subject,
    chrom="chr.*?",
    ":",
    chromStart=".*?", 
    "-",
    chromEnd="[0-9,]*")
  expected <- cbind(
    chrom=c("chr10", "chrNA", NA, NA, "chr1"),
    chromStart=c("213,054,000", "111,000", NA, NA, "110"),
    chromEnd=c("213,055,000", "222,000", NA, NA, "111"))
  rownames(expected) <- names(subject)
  expect_identical(computed, expected)
})

test_that("str_match_variable returns data.frame", {
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  computed <- str_match_variable(
    subject, 
    chrom="chr.*?",
    ":",
    chromStart=".*?", keep.digits,
    "-",
    chromEnd="[0-9,]*", keep.digits)
  expected <- data.frame(
    chrom=c("chr10", "chrNA", NA, NA, "chr1"),
    chromStart=as.integer(c(213054000, 111000, NA, NA, 110)),
    chromEnd=as.integer(c(213055000, 222000, NA, NA, 111)),
    stringsAsFactors=FALSE)
  rownames(expected) <- names(subject)
  expect_equivalent(computed, expected)
})

test_that("str_match_all_variable returns character matrix", {
  computed <- str_match_all_variable(
    subject, 
    chrom="chr.*?",
    ":",
    chromStart=".*?", 
    "-",
    chromEnd="[0-9,]*")
  r <- function(chrom, chromStart, chromEnd){
    cbind(chrom=chrom, chromStart=chromStart, chromEnd=chromEnd)
  }
  expected <- rbind(
    r("chr10", "213,054,000", "213,055,000"),
    r("chrNA", "111,000", "222,000"),
    r("chr1", "110", "111"),
    r("chr2", "220", "222"))
  expect_identical(computed, expected)
})

test_that("str_match_all_variable removes missing subjects", {
  computed <- str_match_all_variable(
    subject, 
    "(?<na>NA)")
  ## There should be only one NA (not two) because chrNA matches but
  ## the missing NA subject should be removed.
  expect_identical(computed, cbind(na="NA"))
})

test_that("str_match_all_variable returns data.frame", {
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
  computed <- str_match_all_variable(
    subject, 
    chrom="chr.*?",
    ":",
    chromStart=".*?", keep.digits,
    "-",
    chromEnd="[0-9,]*", keep.digits)
  expected <- rbind(
    data.frame(
      chrom="chr10", chromStart=213054000L, chromEnd=213055000L,
      stringsAsFactors=FALSE),
    data.frame(
      chrom="chrNA", chromStart=111000L, chromEnd=222000L,
      stringsAsFactors=FALSE),
    data.frame(
      chrom=c("chr1", "chr2"),
      chromStart=as.integer(c("110", "220")),
      chromEnd=as.integer(c("111", "222")),
      stringsAsFactors=FALSE))
  expect_identical(computed, expected)
})

test_that("str_match_variable errors for one argument", {
  expect_error({
    str_match_variable("foo")
  }, "must have at least two arguments: subject, name=pattern, fun, ...")
})

test_that("str_match_all_variable errors for one argument", {
  expect_error({
    str_match_all_variable("foo")
  }, "must have at least two arguments: subject, name=pattern, fun, ...")
})

test_that("str_match_variable errors for multi-dim patterns", {
  expect_error({
    str_match_variable("foo", c("bar", "baz"))
  }, "patterns must be character vectors of length 1")
})

test_that("str_match_all_variable errors for multi-dim patterns", {
  expect_error({
    str_match_all_variable("foo", c("bar", "baz"))
  }, "patterns must be character vectors of length 1")
})

test_that("str_match_variable errors for 0-length patterns", {
  expect_error({
    str_match_variable("foo", character())
  }, "patterns must be character vectors of length 1")
})

test_that("str_match_all_variable errors for 0-length patterns", {
  expect_error({
    str_match_all_variable("foo", character())
  }, "patterns must be character vectors of length 1")
})

test_that("str_match_variable errors for non char/fun args", {
  expect_error({
    str_match_variable("foo", "bar", 1)
  }, "arguments must be character (subject/patterns) or functions (for converting extracted character vectors to other types)", fixed=TRUE)
})

test_that("str_match_all_variable errors for non char/fun args", {
  expect_error({
    str_match_all_variable("foo", "bar", 1)
  }, "arguments must be character (subject/patterns) or functions (for converting extracted character vectors to other types)", fixed=TRUE)
})

test_that("str_match_variable errors for two funs in a row", {
  expect_error({
    str_match_variable("foo", g="bar", as.integer, as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_that("str_match_all_variable errors for two funs in a row", {
  expect_error({
    str_match_all_variable("foo", g="bar", as.integer, as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_that("str_match_variable errors for fun at start", {
  expect_error({
    str_match_variable("foo", as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_that("str_match_all_variable errors for fun at start", {
  expect_error({
    str_match_all_variable("foo", as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_that("str_match_variable errors for NA pattern", {
  expect_error({
    str_match_variable("foo", g="bar", NA_character_, "baz")
  }, "patterns must not be missing/NA")
})

test_that("str_match_all_variable errors for NA pattern", {
  expect_error({
    str_match_all_variable("foo", g="bar", NA_character_, "baz")
  }, "patterns must not be missing/NA")
})
