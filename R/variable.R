str_match_all_variable <- structure(function
### Extract each occurance of a named capture regex pattern from one
### subject string.
(...
### subject, name1=pattern1, fun1, etc, which creates the regex
### (?<name1>pattern1) and uses fun1 for conversion. The first
### argument must be the subject character vector. We treat elements
### of subject as separate lines; i.e. we do the regex matching on the
### single subject string formed by pasting together the subject
### character vector using newlines as the separator. The other
### arguments specify the regular expression pattern and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R, we will add a
### named capture group (?P<name>pattern) in the regex. All patterns
### are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
){
  L <- variable_args_list(...)
  subject <- paste(
    L$subject.vec[!is.na(L$subject.vec)],
    collapse="\n")
  str_match_all_named(subject, L$pattern, L$fun.list)[[1]]
### matrix or data.frame with one row for each match, and one column
### for each named group, see ?str_match_all_named for details.
}, ex=function(){

  library(namedCapture)
  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  ## str_match_all_variable treats elements of subject as separate
  ## lines (and ignores NA elements). Named arguments are used to
  ## create named capture groups, and conversion functions such as
  ## keep.digits are used to convert the previously named group.
  (match.df <- str_match_all_variable(
    chr.pos.vec,
    chrom="chr.*?",
    ":",
    chromStart=".*?", keep.digits,
    "-",
    chromEnd="[0-9,]*", keep.digits))
  str(match.df)

})

str_match_variable <- structure(function
### Extract the first occurance of a named capture regex pattern from
### each of several subject strings.
(...
### subject, name1=pattern1, fun1, etc, which creates the regex
### (?P<name1>pattern1) and uses fun1 for conversion. The first
### argument must be the subject character vector. The other arguments
### specify the regular expression pattern and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R, we will add a
### named capture group (?P<name>pattern) in the regex. All patterns
### are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
){
  L <- variable_args_list(...)
  str_match_named(L$subject.vec, L$pattern, L$fun.list)
### matrix or data.frame with one row for each subject, and one column
### for each named group, see ?str_match_named for details.
}, ex=function(){

  library(namedCapture)
  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  ## str_match_variable finds the first match in each element of
  ## the subject character vector. Named arguments are used to create
  ## named capture groups, and conversion functions such as
  ## keep.digits are used to convert the previously named group.
  (match.df <- str_match_variable(
    chr.pos.vec,
    chrom="chr.*?",
    ":",
    chromStart=".*?", keep.digits,
    "-",
    chromEnd="[0-9,]*", keep.digits))
  str(match.df)

})

variable_args_list <- function
### Parse the variable-length argument list.
(...
### character vectors or functions (for converting extracted character
### vectors to other types). The first element must be the subject
### character vector, and the second element must be a pattern. All
### patterns must be character vectors of length 1. If the pattern is
### a named argument in R, we will add a name tag in the regex
### pattern. All patterns are pasted together to obtain the final
### pattern used for matching. Each named pattern may be followed by
### at most one function which is used to convert the previous named
### pattern. Patterns may also be lists, which are parsed recursively
### for convenience.
){
  arg.list <- list(...)
  if(length(arg.list) < 2){
    stop(
      "must have at least two arguments: ",
      "subject, name=pattern, fun, ...")
  }
  out.list <- list(
    subject.vec=arg.list[[1]],
    fun.list=list())
  pattern.list <- list()
  var.arg.list <- arg.list[-1]
  prev.name <- NULL
  while(length(var.arg.list)){
    var.arg <- var.arg.list[[1]]
    pattern.name <- names(var.arg.list)[1]
    valid.name <- is.character(pattern.name) && 0 < nchar(pattern.name)
    group.start <- if(valid.name){
      if(is.function(var.arg)){
        stop("functions must not be named, problem: ", pattern.name)
      }
      prev.name <- pattern.name
      paste0("(?P<", pattern.name, ">")
    }else{
      "(?:"
    }
    var.arg.list <- var.arg.list[-1]
    if(is.character(var.arg)){
      if(length(var.arg) != 1){
        print(var.arg)
        stop("patterns must be character vectors of length 1")
      }
      if(is.na(var.arg)){
        stop("patterns must not be missing/NA")
      }
      pattern.list[[length(pattern.list)+1L]] <- if(valid.name){
        paste0(group.start, var.arg, ")")
      }else{
        var.arg
      }
    }else if(is.function(var.arg)){
      if(is.null(prev.name)){
        stop(
          "too many functions; ",
          "up to one function may follow each named pattern")
      }
      out.list$fun.list[[prev.name]] <- var.arg
      prev.name <- NULL
    }else if(is.list(var.arg)){
      var.arg.list <- c(group.start, var.arg, ")", var.arg.list)
    }else{
      print(var.arg)
      stop("arguments must be character (subject/patterns), functions (for converting extracted character vectors to other types), or list (parsed recursively)")
    }
  }
  out.list$pattern <- paste(pattern.list, collapse="")
  if(length(out.list$fun.list)==0){
    out.list$fun.list <- NULL
  }
  out.list
### List with three named elements: subject.vec is the subject
### character vector, pattern is the regular expression string, and
### fun.list is a list of conversion functions.
}
