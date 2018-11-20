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




\examples{

library(namedCapture)
chr.pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000-222,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
chr.pos.pattern <- paste0(
  "(?<chrom>chr.*?)",
  ":",
  "(?<chromStart>.*?)",
  "-",
  "(?<chromEnd>[0-9,]*)")
## Specifying a list of conversion functions means that str_match_*
## should convert the matched groups from character to whatever is
## returned by those functions.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
(match.df <- str_match_named(chr.pos.vec, chr.pos.pattern, conversion.list))
str(match.df)

}
