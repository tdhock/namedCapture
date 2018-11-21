df_match_variable <- structure(function
### Use str_match_variable on each column/pattern indicated in
### ... (argument names are interpreted as column names of subject).
(subject,
### data.frame with character columns of subjects for matching.
  ...
### name1=pattern1 etc. See ?short_arg_list for details.
){
  if(!is.data.frame(subject)){
    stop("subject must be a data.frame with character columns to match")
  }
  col.pattern.list <- list(...)
  if(length(col.pattern.list)==0)stop("no patterns specified in ...")
  valid.name <- names(col.pattern.list) %in% names(subject)
  if(is.null(names(col.pattern.list)) || any(!valid.name)){
    stop("each pattern in ... must be named using a column name of subject")
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
