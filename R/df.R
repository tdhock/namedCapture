df_match_variable <- structure(function # First match from every row, variable argument syntax
### Extract text from several columns of a data.frame, using a
### different named capture regular expression for each column. Uses
### str_match_variable on each column/pattern indicated in
### ... -- argument names are interpreted as column names of subject;
### argument values are passed as the pattern to
### str_match_variable.
(# can NOT have subject arg outside of dots (column named subject)
  ...
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
    stop(
      "no patterns specified in ...; ",
      "must specify subjectColName=list(groupName=pattern, etc), etc")
  }
  valid.name <- names(col.pattern.list) %in% names(subject)
  invalid.vec <- names(col.pattern.list)[!valid.name]
  if(is.null(names(col.pattern.list)) || length(invalid.vec)){
    stop("named args (", paste(invalid.vec, collapse=", "),
         ") not found in subject column names (", paste(names(subject), collapse=", "),
         "); each pattern in ... must be named using a column name of subject")
  }
  if(names(all.arg.list)[[1]] != ""){
    stop("first argument (subject data.frame) should not be named")
  }
  name.tab <- table(names(col.pattern.list))
  if(any(bad <- 1 < name.tab)){
    stop(
      "each argument name should be unique, problems: ",
      paste(names(name.tab)[bad], collapse=", "))
  }
  out <- subject
  name.group.used <- FALSE
  for(col.name in names(col.pattern.list)){
    subject.names <- if(.row_names_info(subject) > 0L){
      attr(subject, "row.names")
    }
    subject.vec <- structure(subject[[col.name]], names=subject.names)
    col.arg.list <- c(list(subject.vec), col.pattern.list[[col.name]])
    m <- do.call(str_match_variable, col.arg.list)
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
    if(is.character(colnames(m))){
      colnames(m) <- paste0(col.name, ".", colnames(m))
    }
    out <- cbind(out, m, stringsAsFactors=FALSE)
  }
  out
### data.frame with same number of rows as subject, with an additional
### column for each named capture group specified in ...  (actually
### the value is created via base::cbind so if subject is something else
### like a data.table::data.table then the value is too).
}, ex=function(){

  ## The JobID column can be match with a complicated regular
  ## expression, that we will build up from small sub-pattern list
  ## variables that are easy to understand independently.
  (sacct.df <- data.frame(
    JobID = c(
      "13937810_25", "13937810_25.batch",
      "13937810_25.extern", "14022192_[1-3]", "14022204_[4]"),
    Elapsed = c(
      "07:04:42", "07:04:42", "07:04:49",
      "00:00:00", "00:00:00"),
    stringsAsFactors=FALSE))

  ## Just match the end of the range.
  int.pattern <- list("[0-9]+", as.integer)
  end.pattern <- list(
    "-",
    task_end=int.pattern)
  namedCapture::df_match_variable(sacct.df, JobID=end.pattern)

  ## Match the whole range inside square brackets.
  range.pattern <- list(
    "[[]",
    task_start=int.pattern,
    end.pattern, "?", #end is optional.
    "[]]")
  namedCapture::df_match_variable(sacct.df, JobID=range.pattern)

  ## Match either a single task ID or a range, after an underscore.
  task.pattern <- list(
    "_",
    list(
      task_id=int.pattern,
      "|",#either one task(above) or range(below)
      range.pattern))
  namedCapture::df_match_variable(sacct.df, JobID=task.pattern)

  ## Match type suffix alone.
  type.pattern <- list(
    "[.]",
    type=".*")
  namedCapture::df_match_variable(sacct.df, JobID=type.pattern)

  ## Match task and optional type suffix.
  task.type.pattern <- list(
    task.pattern,
    type.pattern, "?")
  namedCapture::df_match_variable(sacct.df, JobID=task.type.pattern)

  ## Match full JobID and Elapsed columns.
  (task.df <- namedCapture::df_match_variable(
    sacct.df,
    JobID=list(
      job=int.pattern,
      task.type.pattern),
    Elapsed=list(
      hours=int.pattern,
      ":",
      minutes=int.pattern,
      ":",
      seconds=int.pattern)))
  str(task.df)

})
