library(testthat)
library(namedCapture)
context("NA subjects")

N <- 1e5
subject.vec <- c("this will match", rep(NA, N))
pattern <- paste0("(?<name>ill)(?<rest>.*)")

test_that("NA subjects do not cause warnings", {
  ## Sometimes the C code underly regexpr will return large ints in
  ## capture start/length for subjects with missing values, and in
  ## that case we were seeing the warning "NAs introduced by coercion
  ## to integer range." but sometimes (randomly) it did not cause a
  ## warning.
  expect_silent({
    computed.mat <- str_match_named(subject.vec, pattern)
  })
  expected.mat <- matrix(
    c(" match", rep(NA, N)),
    dimnames=list(c("ill", rep(NA, N)), "rest"))
  expect_identical(computed.mat, expected.mat)
})
