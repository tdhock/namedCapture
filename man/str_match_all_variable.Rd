\name{str_match_all_variable}
\alias{str_match_all_variable}
\title{str match all variable}
\description{Extract each occurance of a named capture regex pattern from one
subject string.}
\usage{str_match_all_variable(...)}
\arguments{
  \item{\dots}{subject, name1=pattern1, fun1, etc, which creates the regex
(?<name1>pattern1) and uses fun1 for conversion. The first
argument must be the subject character vector. We treat elements
of subject as separate lines; i.e. we do the regex matching on the
single subject string formed by pasting together the subject
character vector using newlines as the separator. The other
arguments specify the regular expression pattern and must be
character/function/list. All patterns must be character vectors of
length 1. If the pattern is a named argument in R, we will add a
named capture group (?<name>pattern) in the regex. All patterns
are pasted together to obtain the final pattern used for
matching. Each named pattern may be followed by at most one
function which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
}

\value{matrix or data.frame with one row for each match, and one column
for each named group, see ?str_match_all_named for details.}

\author{Toby Dylan Hocking}




\examples{

library(namedCapture)
chr.pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000-222,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
## str_match_all_variable treats elements of subject as separate
## lines (and ignores NA elements). Named arguments are used to
## create named capture groups, and conversion functions such as
## keep.digits are used to convert the previously named group.
(match.df <- str_match_all_variable(
  chr.pos.vec, 
  chrom="chr.*?",
  ":",
  chromStart=".*?", keep.digits,
  "-",
  chromEnd="[0-9,]*", keep.digits))
str(match.df)

}
