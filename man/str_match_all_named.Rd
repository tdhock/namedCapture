\name{str_match_all_named}
\alias{str_match_all_named}
\title{str match all named}
\description{Parse several occurances of pattern from each of several subject
strings using named capturing regular expressions.}
\usage{str_match_all_named(subject.vec, pattern, type.list = NULL)}
\arguments{
  \item{subject.vec}{character vector of subjects.}
  \item{pattern}{named capture regular expression (character vector of length 1).}
  \item{type.list}{named list of functions to apply to captured groups.}
}

\value{A list of data.frames with one row for each subject and one column
for each capture group if type.list is a list of
functions. Otherwise a list of character matrices. If pattern
contains a group named "name" then it will not be returned as a
column, and will instead be used for the rownames of the
data.frames or matrices. If subject.vec has names, they will be
used as the names of the returned list.}

\author{Toby Dylan Hocking}





