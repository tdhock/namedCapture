### Error if subject.vec or pattern incorrect type.
check_subject_pattern <- function(subject.vec, pattern){
  if(!(
    is.character(subject.vec) &&
    0 < length(subject.vec)
  )){
    str(subject.vec)
    stop("subject.vec should be a character vector with length>0")
  }
  if(!(
    is.character(pattern) &&
    length(pattern) == 1 &&
    !is.na(pattern)
  )){
    str(pattern)
    stop("pattern should be a character scalar (not missing/NA)")
  }
}

### Extract capture group columns from match.mat, stop if any are
### un-named, and assign optional groups to "".
only_captures <- function(match.mat, pattern){
  group.mat <- match.mat[, -1, drop=FALSE]
  un.named.group <- grepl("^[.]", colnames(group.mat))
  if(any(un.named.group) || ncol(group.mat)==0){
    stop_for_names(pattern)
  }
  missing.match <- is.na(match.mat[,1])
  group.mat[is.na(group.mat) & !missing.match] <- "" #optional groups
  group.mat
}

### Informative error message when named group(s) missing.
stop_for_names <- function(pattern){
  stop("\n", pattern, '
pattern (above) must contain named capture groups (?P<name>subpattern);
make one using namedCapture::str_*_variable(subject.vec, name="subpattern") or
namedCapture::df_match_variable(subject.df, colName=list(name="subpattern"))')
}

str_match_named <- structure(function # First match from multiple subjects, three argument syntax
### Extract the first match of pattern from each element of
### subject.vec using a named capture regular expression. Uses
### re2r::re2_match if engine is RE2, otherwise uses
### base::regexpr with \code{perl=TRUE}. This function is mostly for
### internal use; most users should use str_match_variable instead.
(subject.vec,
### character vector of subjects.
 pattern,
### named capture regular expression (character vector of length 1).
 type.list=NULL
### named list of functions to apply to captured groups, in order to
### create non-character (typically numeric) columns in the result.
){
  check_subject_pattern(subject.vec, pattern)
  m <- if(engine()=="RE2"){
    re2.mat <- re2r::re2_match(subject.vec, pattern)
    only_captures(re2.mat, pattern)
  }else{
    vec.with.attrs <- regexpr(pattern, subject.vec, perl=TRUE)
    no.match <- vec.with.attrs == -1 | is.na(subject.vec)
    capture.names <- names_or_error(vec.with.attrs, pattern)
    first <- attr(vec.with.attrs, "capture.start")
    first[no.match] <- NA
    last <- attr(vec.with.attrs, "capture.length")-1+first
    last[no.match] <- NA
    subs <- substring(subject.vec, first, last)
    matrix(
      subs, length(subject.vec), length(capture.names),
      dimnames=list(NULL, capture.names))
  }
  rownames(m) <- names(subject.vec)
  apply_type_funs(m, type.list)
### A data.frame with one row for each subject and one column for each
### capture group if type.list is a list of functions. Otherwise a
### character matrix. If subject.vec has names then they will be used
### for the rownames of the returned data.frame or character
### matrix. Otherwise if pattern has a group named "name" then it will
### not be returned as a column, and will instead be used for the
### rownames.
}, ex=function(){

  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  chr.pos.pattern <- paste0(
    "(?P<chrom>chr.*?)",
    ":",
    "(?P<chromStart>.*?)",
    "-",
    "(?P<chromEnd>[0-9,]*)")
  ## Specifying a list of conversion functions means that str_match_*
  ## should convert the matched groups from character to whatever is
  ## returned by those functions.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
  (match.df <- namedCapture::str_match_named(chr.pos.vec, chr.pos.pattern, conversion.list))
  str(match.df)

})

str_match_all_named <- structure(function # All matches from multiple subjects, three argument syntax
### Extract all matches of pattern from each element of subject.vec
### using named capturing regular expressions. Uses
### re2r::re2_match_all if engine is RE2, otherwise uses
### base::gregexpr with \code{perl=TRUE}.  For the common case of
### extracting all matches of a regex from a multi-line text file,
### please use str_match_all_variable instead.
(subject.vec,
### character vector of subjects.
 pattern,
### named capture regular expression (character vector of length 1).
 type.list=NULL
### named list of functions to apply to captured groups, in order to
### create non-character (typically numeric) columns in the result.
 ){
  check_subject_pattern(subject.vec, pattern)
  unconverted.list <- list()
  no.match.mat <- matrix(character(), nrow=0)
  if(engine()=="RE2"){
    re2.list <- re2r::re2_match_all(subject.vec, pattern)
    for(i in seq_along(subject.vec)){
      subject.is.na <- is.na(subject.vec[[i]])
      re2.mat <- re2.list[[i]]
      no.match <- nrow(re2.mat)==0
      unconverted.list[[i]] <- if(no.match || subject.is.na){
        no.match.mat
      }else{
        only_captures(re2.mat, pattern)
      }
    }
  }else{
    parsed <- gregexpr(pattern, subject.vec, perl=TRUE)
    for(i in seq_along(subject.vec)){
      vec.with.attrs <- parsed[[i]]
      first.start <- vec.with.attrs[1]
      subject.is.na <- is.na(first.start)
      no.match <- first.start == -1
      if(no.match || subject.is.na){
        m <- no.match.mat
      }else{
        first <- attr(vec.with.attrs, "capture.start")
        last <- attr(vec.with.attrs, "capture.length")-1+first
        subs <- substring(subject.vec[i], first, last)
        m <- matrix(subs, nrow=nrow(first))
        colnames(m) <- names_or_error(vec.with.attrs, pattern)
      }
      unconverted.list[[i]] <- m
    }
  }
  result.list <- lapply(unconverted.list, apply_type_funs, type.list)
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

  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  chr.pos.pattern <- paste0(
    "(?P<chrom>chr.*?)",
    ":",
    "(?P<chromStart>.*?)",
    "-",
    "(?P<chromEnd>[0-9,]*)")
  ## Specifying a list of conversion functions means that str_match_*
  ## should convert the matched groups from character to whatever is
  ## returned by those functions.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
  ## Use str_match_all_named to get ALL matches in each subject (not
  ## just the first match).
  (match.df.list <- namedCapture::str_match_all_named(
    chr.pos.vec, chr.pos.pattern, conversion.list))
  str(match.df.list)
  ## If there is a capture group named "name" then it will be used for
  ## the rownames of the result.
  name.value.vec <- c(
    H3K27me3="  sampleType=monocyte   assayType=H3K27me3    cost=5",
    H3K27ac="sampleType=monocyte assayType=H3K27ac",
    H3K4me3=" sampleType=Myeloidcell cost=30.5  assayType=H3K4me3")
  name.value.pattern <- paste0(
    "(?P<name>[^ ]+?)",
    "=",
    "(?P<value>[^ ]+)")
  namedCapture::str_match_all_named(name.value.vec, name.value.pattern)

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
    name.vec <- match.mat[, "name"]
    match.df <- data.frame(match.mat, stringsAsFactors=FALSE)
    rownames(match.df) <- 1:nrow(match.df)
    if(any(gone <- is.na(name.vec))){
      print(match.df[gone, ])
      stop("the 'name' group should not be missing/NA")
    }
    name.tab <- table(name.vec)
    not.uniq <- name.tab[1 < name.tab]
    if(length(not.uniq)){
      print(match.df[name.vec %in% names(not.uniq), ])
      stop("capture group named 'name' must be unique")
    }
    rownames(match.mat) <- name.vec
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
            "list(group.name=function(character.vector)atomic.vector)")
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
(vec.with.attrs,
### Output from g?regexpr.
  pattern
 ){
  capture.names <- attr(vec.with.attrs, "capture.names")
  if(!is.character(capture.names) || any(capture.names == "")){
    stop_for_names(pattern)
  }
  capture.names
### Character vector.
}
