str_match_named <- structure(function
### Parse the first occurance of pattern from each of several subject
### strings using a named capture regular expression.
(subject.vec,
### character vector of subjects.
 pattern,
### named capture regular expression (character vector of length 1).
 type.list=NULL
### named list of functions to apply to captured groups.
 ){
  stopifnot(is.character(subject.vec))
  stopifnot(0 < length(subject.vec))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  vec.with.attrs <- regexpr(pattern, subject.vec, perl=TRUE)
  no.match <- vec.with.attrs == -1 | is.na(subject.vec)
  capture.names <- names_or_error(vec.with.attrs)
  first <- attr(vec.with.attrs, "capture.start")
  first[no.match] <- NA
  last <- attr(vec.with.attrs, "capture.length")-1+first
  last[no.match] <- NA
  subs <- substring(subject.vec, first, last)
  m <- matrix(subs, length(subject.vec), length(capture.names),
              dimnames=list(names(subject.vec), capture.names))
  apply_type_funs(m, type.list)
### A data.frame with one row for each subject and one column for each
### capture group if type.list is a list of functions. Otherwise a
### character matrix. If subject.vec has names then they will be used
### for the rownames of the returned data.frame or character
### matrix. Otherwise if there is a group named "name" then it will
### not be returned as a column, and will instead be used for the
### rownames.
}, ex=function(){
  library(namedCapture)
  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  chr.pos.pattern <- paste0(
    "(?<chrom>chr.*?)",
    ":",
    "(?<chromStart>.*?)",
    "-",
    "(?<chromEnd>[0-9,]*)")
  ## Specifying a list of conversion functions means that str_match_*
  ## should convert the matched groups from character to whatever is
  ## returned by those functions.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
  (match.df <- str_match_named(chr.pos.vec, chr.pos.pattern, conversion.list))
  str(match.df)
})

str_match_all_named <- structure(function
### Parse several occurances of pattern from each of several subject
### strings using named capturing regular expressions.
(subject.vec,
### character vector of subjects.
 pattern,
### named capture regular expression (character vector of length 1).
 type.list=NULL
### named list of functions to apply to captured groups.
 ){
  stopifnot(is.character(subject.vec))
  stopifnot(0 < length(subject.vec))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- gregexpr(pattern, subject.vec, perl=TRUE)
  result.list <- list()
  for(i in seq_along(parsed)){
    vec.with.attrs <- parsed[[i]]
    first.start <- vec.with.attrs[1]
    no.match <- first.start == -1
    subject.is.na <- is.na(first.start)
    if(no.match || subject.is.na){
      m <- matrix(character(), nrow=0)
    }else{
      first <- attr(vec.with.attrs, "capture.start")
      last <- attr(vec.with.attrs, "capture.length")-1+first
      subs <- substring(subject.vec[i], first, last)
      m <- matrix(subs, nrow=nrow(first))
      colnames(m) <- names_or_error(vec.with.attrs)
    }
    result.list[[i]] <- apply_type_funs(m, type.list)
  }
  names(result.list) <- names(subject.vec)
  result.list
### A list of data.frames with one row for each subject and one column
### for each capture group if type.list is a list of
### functions. Otherwise a list of character matrices. If pattern
### contains a group named "name" then it will not be returned as a
### column, and will instead be used for the rownames of the
### data.frames or matrices. If subject.vec has names, they will be
### used as the names of the returned list.
}, ex=function(){
  library(namedCapture)
  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  chr.pos.pattern <- paste0(
    "(?<chrom>chr.*?)",
    ":",
    "(?<chromStart>.*?)",
    "-",
    "(?<chromEnd>[0-9,]*)")
  ## Specifying a list of conversion functions means that str_match_*
  ## should convert the matched groups from character to whatever is
  ## returned by those functions.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
  ## Use str_match_all_named to get ALL matches in each subject (not
  ## just the first match).
  (match.df.list <- str_match_all_named(
    chr.pos.vec, chr.pos.pattern, conversion.list))
  str(match.df.list)
  ## If there is a capture group named "name" then it will be used for
  ## the rownames of the result.
  name.value.vec <- c(
    H3K27me3="  sampleType=monocyte   assayType=H3K27me3    cost=5",
    H3K27ac="sampleType=monocyte assayType=H3K27ac",
    H3K4me3=" sampleType=Myeloidcell cost=30.5  assayType=H3K4me3")
  name.value.pattern <- paste0(
    "(?<name>[^ ]+?)",
    "=",
    "(?<value>[^ ]+)")
  str_match_all_named(name.value.vec, name.value.pattern)
})

apply_type_funs <- function
### Convert columns of match.mat using corresponding functions from
### type.list.
(match.mat,
### character matrix (matches X groups).
 type.list
### named list of functions to apply to captured groups.
 ){
  stopifnot(is.character(match.mat))
  stopifnot(is.matrix(match.mat))
  if(is.null(rownames(match.mat)) && "name" %in% colnames(match.mat)){
    rownames(match.mat) <- match.mat[, "name"]
    match.mat <- match.mat[, colnames(match.mat) != "name", drop=FALSE]
  }
  if(is.list(type.list)){
    df <- data.frame(match.mat, stringsAsFactors=FALSE)
    for(col.name in names(type.list)){
      if(col.name %in% names(df)){
        type.fun <- type.list[[col.name]]
        tryCatch({
          fun.result <- type.fun(df[[col.name]])
        }, error=function(e){
          stop(
            "type.list must be ",
            "list(group.name=function(character.vector)any.vector)")
        })
        if(!is.atomic(fun.result)){
          stop(col.name, " type.list function must return atomic vector")
        }
        if(length(fun.result) != nrow(df)){
          stop(
            col.name,
            " type.list function returned vector of length ",
            length(fun.result),
            " but expected length ",
            nrow(df))
        }
        df[[col.name]] <- fun.result
      }
    }
    df
  }else{
    match.mat
  }
### If type.list is a list of functions, then return a data.frame
### whose columns are defined by calling the functions in type.list on
### the corresponding column of match.mat. Otherwise just return a
### character matrix. If match.mat does not already have rownames, and
### it has a column named "name", then that column will be used for
### the rownames, and that column will not be returned.
}

names_or_error <- function
### Extract capture group names. Stop with an error if there are no
### capture groups, or if there are any capture groups without names.
(vec.with.attrs
### Output from g?regexpr.
 ){
  capture.names <- attr(vec.with.attrs, "capture.names")
  if(!is.character(capture.names) || any(capture.names == "")){
    stop("pattern must contain named capture groups (?<name>subpattern)")
  }
  capture.names
### Character vector.
}
