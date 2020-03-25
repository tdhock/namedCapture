library(testthat)
library(namedCapture)
context("engine")
source(system.file("test_engines.R", package="namedCapture", mustWork=TRUE), local=TRUE)

subject <- "chr10:1-2"
pattern <- "(?<chrom>.*?):"
expected.mat <- cbind(chrom="chr10")
test_engines("(?<name>pattern) syntax works (PCRE) or errors (RE2)", {
  do_match <- function()str_match_named(subject, pattern)
  if(engine()=="RE2"){
    expect_error({
      do_match()
    }, "bad perl operator: (?<", fixed=TRUE)
  }else{#PCRE
    computed.mat <- do_match()
    expect_identical(computed.mat, expected.mat)
  }
})

test_that("engine foobar means use RE2 if available else PCRE", {
  old.opt <- options(namedCapture.engine="foobar")
  expected.engine <- if(RE2.available)"RE2" else "PCRE"
  expect_identical(engine(), expected.engine)
  options(old.opt)
})

