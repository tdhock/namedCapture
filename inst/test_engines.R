available.engines <- c(
  "PCRE",
  if(requireNamespace("re2r", quietly=TRUE))"RE2")
test_engines <- function(desc, ...){
  for(e in available.engines){
    old.opt <- options(namedCapture.engine=e)
    test_that(paste(e, desc), ...)
    options(old.opt)
  }
}
