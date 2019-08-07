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

}
