str_match_all_variable <- structure(function # All matches from one subject, variable argument syntax
### Extract all matches of a named capture regex pattern from one
### subject string.
### It is for the common case of extracting
### all matches of a regex from a single multi-line text file subject;
### for other subjects, str_match_all_named can be used to find all matches.
### This function uses
### variable_args_list to analyze the arguments and
### str_match_all_named to perform the matching.
(subject.vec,
### The subject character vector. We treat elements of subject as
### separate lines; i.e. we do the regex matching on the single
### subject string formed by pasting together the subject character
### vector using newlines as the separator.
  ...
### name1=pattern1, fun1, etc, which creates the regex
### (?<name1>pattern1) and uses fun1 for conversion. These other
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
    subject.vec[!is.na(subject.vec)],
    collapse="\n")
  str_match_all_named(subject, L$pattern, L$fun.list)[[1]]
### matrix or data.frame with one row for each match, and one column
### for each named group, see str_match_all_named for details.
}, ex=function(){

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
  int.pattern <- list("[0-9,]+", keep.digits)
  (match.df <- namedCapture::str_match_all_variable(
    chr.pos.vec,
    name="chr.*?",
    ":",
    chromStart=int.pattern,
    "-",
    chromEnd=int.pattern))
  str(match.df)
  match.df["chr1", "chromEnd"]

})

str_match_variable <- structure(function # First match from multiple subjects, variable argument syntax
### Extract the first match of a named capture regex pattern from each
### of several subject strings. This function uses variable_args_list
### to analyze the arguments and str_match_named to perform the
### matching. For the first match in every row of a data.frame, using
### a different regex for each column, use df_match_variable. For all
### matches in one character subject use str_match_all_variable; for
### all matches in several character subjects use str_match_all_named.
(subject.vec,
### The subject character vector.
  ...,
### name1=pattern1, fun1, etc, which creates the regex
### (?P<name1>pattern1) and uses fun1 for conversion. These other arguments
### specify the regular expression pattern and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R, we will add a
### named capture group (?P<name>pattern) in the regex. All patterns
### are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
  nomatch.error=FALSE
### if TRUE, stop with an error if any subject does not match;
### otherwise (default), subjects that do not match are reported as
### missing/NA rows of the result.
){
  L <- variable_args_list(...)
  ##alias<< namedCapture
  df.or.mat <- str_match_named(subject.vec, L$pattern, L$fun.list)
  if(isTRUE(nomatch.error)){
    no.match <- apply(is.na(df.or.mat), 1, all)
    if(any(no.match)){
      print(subject.vec[no.match])
      stop("subjects printed above did not match regex below\n", L$pattern)
    }
  }
  df.or.mat
### matrix or data.frame with one row for each subject, and one column
### for each named group, see str_match_named for details.
}, ex=function(){

  named.subject.vec <- c(
    ten="chr10:213,054,000-213,055,000",
    M="chrM:111,000",
    one="chr1:110-111 chr2:220-222") # two possible matches.
  ## str_match_variable finds the first match in each element of the
  ## subject character vector. Named arguments are used to create
  ## named capture groups, which become column names in the
  ## result. Since the subject is named, those names are used for the
  ## rownames of the result.
  (mat.subject.names <- namedCapture::str_match_variable(
    named.subject.vec,
    chrom="chr.*?",
    ":",
    chromStart="[0-9,]+",
    list( # un-named list becomes non-capturing group.
      "-",
      chromEnd="[0-9,]+"
    ), "?")) # chromEnd is optional.

  ## When no type conversion functions are specified, the result is a
  ## character matrix.
  str(mat.subject.names)

  ## Conversion functions are used to convert the previously named
  ## group, and patterns may be saved in lists for re-use.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  int.pattern <- list("[0-9,]+", keep.digits)
  range.pattern <- list(
    name="chr.*?", # will be used for rownames when subject is un-named.
    ":",
    chromStart=int.pattern,
    list(
      "-",
      chromEnd=int.pattern
    ), "?")

  ## Rownames taken from subject if it has names.
  (df.subject.names <- namedCapture::str_match_variable(
    named.subject.vec, range.pattern))

  ## Conversion functions used to create non-char columns.
  str(df.subject.names)

  ## Rownames taken from name group if subject is un-named.
  namedCapture::str_match_variable(
    unname(named.subject.vec), range.pattern)

  ## NA used to indicate no match or missing subject.
  na.vec <- c(
    nomatch="this will not match",
    missing=NA, # neither will this.
    named.subject.vec)
  namedCapture::str_match_variable(
    na.vec, range.pattern)

})

variable_args_list <- structure(function
### Parse the variable-length argument list used in
### str_match_variable, str_match_all_variable, and
### df_match_variable. This function is mostly intended for internal
### use, but is useful if you want to see the regex pattern generated
### by the variable argument syntax.
(...
### character vectors (for regex patterns) or functions (which specify
### how to convert extracted character vectors to other types). All
### patterns must be character vectors of length 1. If the pattern is
### a named argument in R, we will add a name tag in the regex
### pattern. All patterns are pasted together to obtain the final
### pattern used for matching. Each named pattern may be followed by
### at most one function which is used to convert the previous named
### pattern. Patterns may also be lists, which are parsed recursively
### for convenience.
){
  var.arg.list <- list(...)
  if(length(var.arg.list) < 1){
    stop(
      "pattern must have at least one argument: ",
      "name=pattern, fun, ...")
  }
  fun.list <- list()
  pattern.list <- list()
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
      fun.list[[prev.name]] <- var.arg
      prev.name <- NULL
    }else if(is.list(var.arg)){
      var.arg.list <- c(group.start, var.arg, ")", var.arg.list)
    }else{
      print(var.arg)
      stop("invalid argument printed above; arguments must be character (subject/patterns), functions (for converting extracted character vectors to other types), or list (parsed recursively)")
    }
  }
  ##value<< a list with two named elements
  list(
    fun.list=##<< list of conversion functions or NULL
      if(length(fun.list))fun.list,
    pattern=##<< regular expression string
      paste(pattern.list, collapse="")
  )
  ##end<<
}, ex=function(){

  pos.pattern <- list("[0-9]+", as.integer)
  namedCapture::variable_args_list(
    "some subject",
    chrom="chr.*?",
    ":",
    chromStart=pos.pattern,
    list(
      "-",
      chromEnd=pos.pattern
    ), "?")

})
