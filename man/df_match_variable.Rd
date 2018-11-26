\name{df_match_variable}
\alias{df_match_variable}
\title{df match variable}
\description{Extract text from several columns of a data.frame, using a
different named capture regular expression for each column. Uses
str_match_variable on each column/pattern indicated in
... (argument names are interpreted as column names of subject;
argument values are passed as the pattern to
str_match_variable). Each pattern is typically specified as a list
of length-1 character vectors (pasted together to obtain a regular
expression) and functions (used to convert the captured text to
other types).}
\usage{df_match_variable(...)}
\arguments{
  \item{\dots}{First (un-named) argument should be a data.frame with character
columns of subjects for matching. The other arguments should be
name1=pattern1 etc. See ?short_arg_list for details.}
}

\value{data.frame with same number of rows as subject, with an additional
column for each named capture group specified in ...  (actually
the value is created via cbind so if subject is something else
like a data.table then the value is too).}

\author{Toby Dylan Hocking}




\examples{

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

}
