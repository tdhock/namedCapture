\name{df_match_variable}
\alias{df_match_variable}
\title{df match variable}
\description{Use str_match_variable on each column/pattern indicated in
... (argument names are interpreted as column names of subject).}
\usage{df_match_variable(subject, ...)}
\arguments{
  \item{subject}{data.frame with character columns of subjects for matching.}
  \item{\dots}{name1=pattern1 etc. See ?short_arg_list for details.}
}

\value{data.frame with same number of rows as subject, with an additional
column for each named capture group specified in ...  (actually
the value is created via cbind so if subject is something else
like a data.table then the value is too).}

\author{Toby Dylan Hocking}





