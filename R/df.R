df_match_variable <- structure(function 
### Extract text from several columns of a data.frame, using a
### different named capture regular expression for each column. Uses
### str_match_variable on each column/pattern indicated in
### ... (argument names are interpreted as column names of subject;
### argument values are passed as the pattern to
### str_match_variable). Each pattern is typically specified as a list
### of length-1 character vectors (pasted together to obtain a regular
### expression) and functions (used to convert the captured text to
### other types).
(...
### First (un-named) argument should be a data.frame with character
### columns of subjects for matching. The other arguments should be
### name1=pattern1 etc. See ?short_arg_list for details.
){
  all.arg.list <- list(...)
  subject <- all.arg.list[[1]]
  if(!is.data.frame(subject)){
    stop("subject must be a data.frame with character columns to match")
  }
  col.pattern.list <- all.arg.list[-1]
  if(length(col.pattern.list)==0)stop("no patterns specified in ...")
  valid.name <- names(col.pattern.list) %in% names(subject)
  invalid.vec <- names(col.pattern.list)[!valid.name]
  if(is.null(names(col.pattern.list)) || length(invalid.vec)){
    stop("named args (", paste(invalid.vec, collapse=", "),
         ") not found in subject column names (", paste(names(subject), collapse=", "),
         "); each pattern in ... must be named using a column name of subject")
  }
  out.list <- list(subject)
  has.rownames <- logical()
  default.rownames <- paste(1:nrow(subject))
  subject.names <- if(identical(rownames(subject), default.rownames)){
    NULL
  }else{
    rownames(subject)
  }
  for(col.name in names(col.pattern.list)){
    subject.vec <- subject[[col.name]]
    names(subject.vec) <- subject.names
    arg.list <- list(subject.vec, col.pattern.list[[col.name]])
    result <- do.call(str_match_variable, arg.list)
    group.names <- colnames(result)
    out <- data.frame(result, stringsAsFactors=FALSE)
    has.rownames[[col.name]] <- !identical(rownames(out), default.rownames)
    names(out) <- paste0(col.name, ".", group.names)
    out.list[[col.name]] <- out
  }
  if(is.null(subject.names) && 1 < sum(has.rownames)){
    stop(
      "only one group named 'name' is allowed; problems: ",
      paste(names(has.rownames)[has.rownames], collapse=", "))
  }
  names(out.list) <- NULL
  do.call(cbind, out.list)
### data.frame with same number of rows as subject, with an additional
### column for each named capture group specified in ...  (actually
### the value is created via cbind so if subject is something else
### like a data.table then the value is too).
}, ex=function(){
  
  library(namedCapture)
  (sacct.df <- data.frame(
    JobID = c(
      "13937810_25", "13937810_25.batch", 
      "13937810_25.extern", "14022192_[1-3]", "14022204_[4]"),
    ExitCode = c("0:0", "0:0", "0:0", "0:0", "0:0"),
    State = c(
      "COMPLETED", "COMPLETED", "COMPLETED",
      "PENDING", "PENDING"),
    MaxRSS = c("", "394960K", "750K", "", ""),
    Elapsed = c(
      "07:04:42", "07:04:42", "07:04:49",
      "00:00:00", "00:00:00"),
    stringsAsFactors=FALSE))
  range.pattern <- list(
    "[[]",
    task1="[0-9]+", as.integer,
    "(?:-",#begin optional end of range.
    taskN="[0-9]+", as.integer,
    ")?", #end is optional.
    "[]]")
  task.pattern <- list(
    "(?:",#begin alternate
    task="[0-9]+", as.integer,
    "|",#either one task(above) or range(below)
    range.pattern,
    ")")#end alternate
  (task.df <- df_match_variable(
    sacct.df,
    JobID=list(
      job="[0-9]+", as.integer,
      "_",
      task.pattern,
      "(?:[.]",
      type=".*",
      ")?"),
    Elapsed=list(
      hours="[0-9]+", as.integer,
      ":",
      minutes="[0-9]+", as.integer,
      ":",
      seconds="[0-9]+", as.integer)))
  str(task.df)
  
})
