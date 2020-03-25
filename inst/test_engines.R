RE2.available <- requireNamespace("re2r", quietly=TRUE)
available.engines <- c("PCRE", if(RE2.available)"RE2")
test_engines <- function(desc, ...){
  for(e in available.engines){
    old.opt <- options(namedCapture.engine=e)
    test_that(paste(e, desc), ...)
    options(old.opt)
  }
}
