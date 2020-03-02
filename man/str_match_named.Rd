\name{str_match_named}
\alias{str_match_named}
\title{First match from multiple subjects, three argument syntax}
\description{Extract the first match of \code{pattern} from each element of
\code{subject.vec} using a named capture regular expression. Uses
\code{\link[re2r]{re2_match}} if \code{\link{engine}} is RE2, otherwise uses
\code{\link[base]{regexpr}} with \code{perl=TRUE}. This function is mostly for
internal use; most users should use \code{\link{str_match_variable}} instead.}
\usage{str_match_named(subject.vec, 
    pattern, type.list = NULL)}
\arguments{
  \item{subject.vec}{character vector of subjects.}
  \item{pattern}{named capture regular expression (character vector of length 1).}
  \item{type.list}{named list of functions to apply to captured groups, in order to
create non-character (typically numeric) columns in the result.}
}

\value{A data.frame with one row for each subject and one column for each
capture group if \code{type.list} is a list of functions. Otherwise a
character matrix. If \code{subject.vec} has names then they will be used
for the rownames of the returned data.frame or character
matrix. Otherwise if \code{pattern} has a group named "name" then it will
not be returned as a column, and will instead be used for the
rownames.}

\author{Toby Dylan Hocking}




\examples{

chr.pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000-222,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
chr.pos.pattern <- paste0(
  "(?P<chrom>chr.*?)",
  ":",
  "(?P<chromStart>.*?)",
  "-",
  "(?P<chromEnd>[0-9,]*)")
## Specifying a list of conversion functions means that str_match_*
## should convert the matched groups from character to whatever is
## returned by those functions.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
conversion.list <- list(chromStart=keep.digits, chromEnd=keep.digits)
(match.df <- namedCapture::str_match_named(chr.pos.vec, chr.pos.pattern, conversion.list))
str(match.df)

}
