namedCapture.engine <- structure(function
### Get current regex engine used by str_match_named and
### str_match_all_named (if no arguments), or set the current engine
### (if an argument is given). The "namedCapture.engine" option is
### used, and can be set by the user,
### i.e. options(namedCapture.engine=e) is the same as
### namedCapture.engine(e). RE2 is used by default if the re2r package
### is available.
(e
### regex engine, either "PCRE" or "RE2" -- RE2 can only be used if
### the re2r package is available.
){
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
}, ex=function(){

  library(namedCapture)
  old.engine <- namedCapture.engine()
  namedCapture.engine("PCRE")
  namedCapture.engine()
  namedCapture.engine("RE2")
  namedCapture.engine()
  namedCapture.engine(old.engine)
  namedCapture.engine()
  
})
