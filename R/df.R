df_match_variable <- function
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
}
