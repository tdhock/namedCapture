namedCapture.engine <- function(e){
  if(missing(e)){
    RE2.available <- requireNamespace("re2r", quietly=TRUE)
    default <- if(RE2.available)"RE2" else "PCRE"
    opt <- getOption("namedCapture.engine", default)
    if(identical(opt, "RE2") && RE2.available){
      "RE2"
    }else{
      "PCRE"
    }
  }else{
    options(namedCapture.engine=e)
  }
}
