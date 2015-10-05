\name{str_match_named}
\alias{str_match_named}
\title{str match named}
\description{Parse the first occurance of pattern from each of several subject
strings using a named capture regular expression.}
\usage{str_match_named(subject.vec, pattern, type.list = NULL)}
\arguments{
  \item{subject.vec}{character vector of subjects.}
  \item{pattern}{named capture regular expression (character vector of length 1).}
  \item{type.list}{named list of functions to apply to captured groups.}
}

\value{A data.frame with one row for each subject and one column for each
capture group if type.list is a list of functions. Otherwise a
character matrix. If subject.vec has names then they will be used
for the rownames of the returned data.frame or character
matrix. Otherwise if there is a group named "name" then it will
not be returned as a column, and will instead be used for the
rownames.}

\author{Toby Dylan Hocking}





