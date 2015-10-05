\name{str_match_all_perl}
\alias{str_match_all_perl}
\title{str match all perl}

\usage{str_match_all_perl(subject.vec, pattern, type.list = NULL)}
\arguments{
  \item{subject.vec}{character vector of subjects.}
  \item{pattern}{named capture regular expression (character vector of length 1).}
  \item{type.list}{named list of functions to apply to captured groups.}
}

\value{A list of data.frames with one row for each subject and one column
for each capture group if type.list is a list of
functions. Otherwise a list of character matrices the first column
of which is the entire match. If the pattern contains a group
named "name" then it will be used for the rownames.}

\author{Toby Dylan Hocking}





