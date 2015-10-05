\name{apply_type_funs}
\alias{apply_type_funs}
\title{apply type funs}
\description{Convert columns of match.mat using corresponding functions from
type.list.}
\usage{apply_type_funs(match.mat, type.list)}
\arguments{
  \item{match.mat}{character matrix (matches X 1 full match + several groups).}
  \item{type.list}{named list of functions to apply to captured groups.}
}

\value{If type.list is a list of functions, then return a data.frame
whose columns are defined by calling the functions in type.list on
the corresponding column of match.mat. Otherwise just return
match.mat. If match.mat has a column named "name" then it will be
used for the rownames.}

\author{Toby Dylan Hocking}





