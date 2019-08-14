engine <- structure(function
### Get current regex engine used by str_match_named and
### str_match_all_named. RE2 is used by default if the re2r package is
### available; otherwise, PCRE is used by default. The user can set
### \code{options(engine="PCRE")} to use PCRE even when
### RE2 is available.
(){
  RE2.available <- requireNamespace("re2r", quietly=TRUE)
  default <- if(RE2.available)"RE2" else "PCRE"
  opt <- getOption("namedCapture.engine", default)
  if(identical(opt, "RE2") && RE2.available){
    "RE2"
  }else if(identical(opt, "PCRE")){
    "PCRE"
  }else{
    default
  }
}, ex=function(){

  namedCapture::engine()
  old.opt <- options(engine="PCRE")
  namedCapture::engine()
  options(old.opt)

})
