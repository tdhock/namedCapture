library(testthat)
library(namedCapture)
context("NA subjects")
source(system.file("test_engines.R", package="namedCapture", mustWork=TRUE), local=TRUE)

N <- 1e5
subject.vec <- c("this will match", rep(NA, N))
pattern <- paste0("(?P<first>ill)(?P<rest>.*)")

test_engines("NA subjects do not cause warnings", {
  ## Sometimes the C code underly regexpr will return large ints in
  ## capture start/length for subjects with missing values, and in
  ## that case we were seeing the warning "NAs introduced by coercion
  ## to integer range." but sometimes (randomly) it did not cause a
  ## warning.
  expect_silent({
    computed.mat <- str_match_named(subject.vec, pattern)
  })
  expected.mat <- matrix(
    c("ill", " match"),
    N+1, 2, byrow=TRUE)
  expected.mat[2:nrow(expected.mat), ] <- NA
  colnames(expected.mat) <- c("first", "rest")
  expect_identical(computed.mat, expected.mat)
})


