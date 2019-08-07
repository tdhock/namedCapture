\name{df_match_variable}
\alias{df_match_variable}
\title{First match from every row, variable argument syntax}
\description{Extract text from several columns of a data.frame, using a
different named capture regular expression for each column. Uses
\code{\link{str_match_variable}} on each column/pattern indicated in
\code{...} -- argument names are interpreted as column names of subject;
argument values are passed as the pattern to
\code{\link{str_match_variable}}.}
\usage{df_match_variable(...)}
\arguments{
  \item{\dots}{
}
}

\value{data.frame with same number of rows as subject, with an additional
column for each named capture group specified in \code{...}  (actually
the value is created via \code{\link[base]{cbind}} so if subject is something else
like a \code{\link[data.table]{data.table}} then the value is too).}

\author{Toby Dylan Hocking}




\examples{

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

}
