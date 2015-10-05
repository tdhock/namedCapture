str_match_named <- function
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
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  vec.with.attrs <- regexpr(pattern, subject.vec, perl=TRUE)
  group.names <- attr(vec.with.attrs, "capture.names")
  first <- attr(vec.with.attrs, "capture.start")
  last <- attr(vec.with.attrs, "capture.length")-1+first
  subs <- substring(subject.vec, first, last)
  m <- matrix(subs, length(subject.vec), length(group.names),
              dimnames=list(names(subject.vec), group.names))
  m[vec.with.attrs == -1, ] <- NA
  apply_type_funs(m, type.list)
### A data.frame with one row for each subject and one column for each
### capture group if type.list is a list of functions. Otherwise a
### character matrix the first column of which is the entire match. If
### the pattern contains a group named "name" then it will be used for
### the rownames.
}

str_match_all_named <- function
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
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- gregexpr(pattern, subject.vec, perl=TRUE)
  result.list <- list()
  for(i in seq_along(parsed)){
    vec.with.attrs <- parsed[[i]]
    names <- attr(vec.with.attrs, "capture.names")
    if(vec.with.attrs[1]==-1 || is.null(names)){
      m <- matrix(character(), nrow=0)
    }else{
      first <- attr(vec.with.attrs, "capture.start")
      last <- attr(vec.with.attrs, "capture.length")-1+first
      subs <- substring(subject.vec[i], first, last)
      m <- matrix(subs, ncol=length(names))
      colnames(m) <- names
    }
    result.list[[i]] <- apply_type_funs(m, type.list)
  }
  names(result.list) <- names(subject.vec)
  result.list
### A list of data.frames with one row for each subject and one column
### for each capture group if type.list is a list of
### functions. Otherwise a list of character matrices the first column
### of which is the entire match. If the pattern contains a group
### named "name" then it will be used for the rownames.
}

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
    df <- data.frame(match.mat)
    for(col.name in names(type.list)){
      if(col.name %in% names(df)){
        type.fun <- type.list[[col.name]]
        df[[col.name]] <- type.fun(df[[col.name]])
      }
    }
    df
  }else{
    match.mat
  }
### If type.list is a list of functions, then return a data.frame
### whose columns are defined by calling the functions in type.list on
### the corresponding column of match.mat. Otherwise just return
### match.mat. If match.mat has a column named "name" then it will be
### used for the rownames, and that column will not be returned.
}
