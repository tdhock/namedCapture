df_match_variable <- structure(function # First match from every row, variable argument syntax
### Extract text from several columns of a data.frame, using a
### different named capture regular expression for each column. Uses
### str_match_variable on each column/pattern indicated in
### ... -- argument names are interpreted as column names of subject;
### argument values are passed as the pattern to
### str_match_variable.
(... # can NOT have subject arg outside of dots (column named subject)
### subject.df, colName1=list(groupName1=pattern1, fun1, etc),
### colName2=list(etc), etc. First (un-named) argument should be a
### data.frame with character columns of subjects for matching. The
### other arguments need to be named (and the names e.g. colName1 and
### colName2 need to be column names of the subject data.frame). The
### other argument values specify the regular expression, and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R, we will add a
### named capture group (?<groupName1>pattern1) in the regex. All
### patterns are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function (e.g. fun1) which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
){
  all.arg.list <- list(...)
  subject <- all.arg.list[[1]]
  if(!is.data.frame(subject)){
    stop("subject must be a data.frame with character columns to match")
  }
  col.pattern.list <- all.arg.list[-1]
  if(length(col.pattern.list)==0){
    stop("no patterns specified in ...")
  }
  valid.name <- names(col.pattern.list) %in% names(subject)
  invalid.vec <- names(col.pattern.list)[!valid.name]
  if(is.null(names(col.pattern.list)) || length(invalid.vec)){
    stop("named args (", paste(invalid.vec, collapse=", "),
         ") not found in subject column names (", paste(names(subject), collapse=", "),
         "); each pattern in ... must be named using a column name of subject")
  }
  out <- subject
  name.group.used <- FALSE
  for(col.name in names(col.pattern.list)){
    subject.names <- if(.row_names_info(subject) > 0L){
      attr(subject, "row.names")
    }      
    subject.vec <- structure(subject[[col.name]], names=subject.names)
    m <- str_match_variable(subject.vec, col.pattern.list[[col.name]])
    has.names <- (
      is.matrix(m) && !is.null(rownames(m))
    ) || (
      is.data.frame(m) && .row_names_info(m) > 0L
    )
    if(is.null(subject.names) && has.names){
      if(name.group.used){
        stop("only one group named 'name' is allowed")
      }else{
        name.group.used <- TRUE
      }
    }
    colnames(m) <- paste0(col.name, ".", colnames(m))
    out <- cbind(out, m, stringsAsFactors=FALSE)
  }
  out
### data.frame with same number of rows as subject, with an additional
### column for each named capture group specified in ...  (actually
### the value is created via base::cbind so if subject is something else
### like a data.table::data.table then the value is too).
}, ex=function(){

  (sacct.df <- data.frame(
    JobID = c(
      "13937810_25", "13937810_25.batch",
      "13937810_25.extern", "14022192_[1-3]", "14022204_[4]"),
    Elapsed = c(
      "07:04:42", "07:04:42", "07:04:49",
      "00:00:00", "00:00:00"),
    stringsAsFactors=FALSE))

  int.pattern <- list("[0-9]+", as.integer)
  range.pattern <- list(
    "[[]",
    task1=int.pattern,
    "(?:-",#begin optional end of range.
    taskN=int.pattern,
    ")?", #end is optional.
    "[]]")
  namedCapture::df_match_variable(sacct.df, JobID=range.pattern)

  task.pattern <- list(
    "_",
    "(?:",#begin alternate
    task=int.pattern,
    "|",#either one task(above) or range(below)
    range.pattern,
    ")")#end alternate
  namedCapture::df_match_variable(sacct.df, JobID=task.pattern)

  if(requireNamespace("future") && interactive())future::plan("multiprocess")
  (task.df <- namedCapture::df_match_variable(
    sacct.df,
    JobID=list(
      job=int.pattern,
      task.pattern,
      "(?:[.]",
      type=".*",
      ")?"),
    Elapsed=list(
      hours=int.pattern,
      ":",
      minutes=int.pattern,
      ":",
      seconds=int.pattern)))
  str(task.df)

})
