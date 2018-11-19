\name{str_match_named_short}
\alias{str_match_named_short}
\title{str match named short}
\description{Extract the first occurance of a named capture regex pattern from
each of several subject strings.}
\usage{str_match_named_short(...)}
\arguments{
  \item{\dots}{subject, name1=pattern1, fun1, etc, which creates the regex
(?<name1>pattern1) and uses fun1 for conversion. See
?short_arg_list for details.}
}

\value{matrix or data.frame with one row for each subject, and one column
for each named group, see ?str_match_named for details.}

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
(match.df <- str_match_named_short(
  subject, 
  chrom="chr.*?",
  ":",
  chromStart=".*?", keep.digits,
  "-",
  chromEnd="[0-9,]*", keep.digits))
str(match.df)

}
