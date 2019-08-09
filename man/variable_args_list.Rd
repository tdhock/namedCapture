\name{variable_args_list}
\alias{variable_args_list}
\title{variable args list}
\description{Parse the variable-length argument list used in
\code{\link{str_match_variable}}, \code{\link{str_match_all_variable}}, and
\code{\link{df_match_variable}}. This function is mostly intended for internal
use, but is useful if you want to see the regex pattern generated
by the variable argument syntax.}
\usage{variable_args_list(...)}
\arguments{
  \item{\dots}{character vectors (for regex patterns) or functions (which specify
how to convert extracted character vectors to other types). All
patterns must be character vectors of length 1. If the pattern is
a named argument in R, we will add a name tag in the regex
pattern. All patterns are pasted together to obtain the final
pattern used for matching. Each named pattern may be followed by
at most one function which is used to convert the previous named
pattern. Patterns may also be lists, which are parsed recursively
for convenience.}
}

\value{a list with two named elements
\item{fun.list}{list of conversion functions or NULL}
\item{pattern}{regular expression string}}

\author{Toby Dylan Hocking}




\examples{

pos.pattern <- list("[0-9]+", as.integer)
namedCapture::variable_args_list(
  "some subject",
  chrom="chr.*?",
  ":",
  chromStart=pos.pattern,
  list(
    "-",
    chromEnd=pos.pattern
  ), "?")

}
