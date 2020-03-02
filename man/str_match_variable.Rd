\name{str_match_variable}
\alias{str_match_variable}
\alias{namedCapture}
\title{First match from multiple subjects, variable argument syntax}
\description{Extract the first match of a named capture regex pattern from each
of several subject strings. This function uses \code{\link{variable_args_list}}
to analyze the arguments and \code{\link{str_match_named}} to perform the
matching. For the first match in every row of a data.frame, using
a different regex for each column, use \code{\link{df_match_variable}}. For all
matches in one character subject use \code{\link{str_match_all_variable}}; for
all matches in several character subjects use \code{\link{str_match_all_named}}.}
\usage{str_match_variable(subject.vec, 
    ..., nomatch.error = FALSE)}
\arguments{
  \item{subject.vec}{The subject character vector.}
  \item{\dots}{name1=pattern1, fun1, etc, which creates the regex
(?P<name1>pattern1) and uses fun1 for conversion. These other arguments
specify the regular expression pattern and must be
character/function/list. All patterns must be character vectors of
length 1. If the pattern is a named argument in R, we will add a
named capture group (?P<name>pattern) in the regex. All patterns
are pasted together to obtain the final pattern used for
matching. Each named pattern may be followed by at most one
function which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
  \item{nomatch.error}{if TRUE, stop with an error if any subject does not match;
otherwise (default), subjects that do not match are reported as
missing/NA rows of the result.}
}

\value{matrix or data.frame with one row for each subject, and one column
for each named group, see \code{\link{str_match_named}} for details.}

\author{Toby Dylan Hocking}




\examples{

named.subject.vec <- c(
  ten="chr10:213,054,000-213,055,000",
  M="chrM:111,000",
  one="chr1:110-111 chr2:220-222") # two possible matches.
## str_match_variable finds the first match in each element of the
## subject character vector. Named arguments are used to create
## named capture groups, which become column names in the
## result. Since the subject is named, those names are used for the
## rownames of the result.
(mat.subject.names <- namedCapture::str_match_variable(
  named.subject.vec,
  chrom="chr.*?",
  ":",
  chromStart="[0-9,]+",
  list( # un-named list becomes non-capturing group.
    "-",
    chromEnd="[0-9,]+"
  ), "?")) # chromEnd is optional.

## When no type conversion functions are specified, the result is a
## character matrix.
str(mat.subject.names)

## Conversion functions are used to convert the previously named
## group, and patterns may be saved in lists for re-use.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
int.pattern <- list("[0-9,]+", keep.digits)
range.pattern <- list(
  name="chr.*?", # will be used for rownames when subject is un-named.
  ":",
  chromStart=int.pattern,
  list(
    "-",
    chromEnd=int.pattern
  ), "?")

## Rownames taken from subject if it has names.
(df.subject.names <- namedCapture::str_match_variable(
  named.subject.vec, range.pattern))

## Conversion functions used to create non-char columns.
str(df.subject.names)

## Rownames taken from name group if subject is un-named.
namedCapture::str_match_variable(
  unname(named.subject.vec), range.pattern)

## NA used to indicate no match or missing subject.
na.vec <- c(
  nomatch="this will not match",
  missing=NA, # neither will this.
  named.subject.vec)
namedCapture::str_match_variable(
  na.vec, range.pattern)

}
