\name{str_match_all_named}
\alias{str_match_all_named}
\title{All matches from multiple subjects, three argument syntax}
\description{Extract all matches of \code{pattern} from each element of \code{subject.vec}
using named capturing regular expressions. Uses
\code{\link[re2r]{re2_match_all}} if \code{\link{engine}} is RE2, otherwise uses
\code{\link[base]{gregexpr}} with \code{perl=TRUE}.  For the common case of
extracting all matches of a regex from a multi-line text file,
please use \code{\link{str_match_all_variable}} instead.}
\usage{str_match_all_named(subject.vec, 
    pattern, type.list = NULL)}
\arguments{
  \item{subject.vec}{character vector of subjects.}
  \item{pattern}{named capture regular expression (character vector of length 1).}
  \item{type.list}{named list of functions to apply to captured groups, in order to
create non-character (typically numeric) columns in the result.}
}

\value{A list of data.frames with one row for each subject and one column
for each capture group if \code{type.list} is a list of
functions. Otherwise a list of character matrices. If \code{pattern}
contains a group named "name" then it will not be returned as a
column, and will instead be used for the rownames of the
data.frames or matrices. If \code{subject.vec} has names, they will be
used as the names of the returned list.}

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
## Use str_match_all_named to get ALL matches in each subject (not
## just the first match).
(match.df.list <- namedCapture::str_match_all_named(
  chr.pos.vec, chr.pos.pattern, conversion.list))
str(match.df.list)
## If there is a capture group named "name" then it will be used for
## the rownames of the result.
name.value.vec <- c(
  H3K27me3="  sampleType=monocyte   assayType=H3K27me3    cost=5",
  H3K27ac="sampleType=monocyte assayType=H3K27ac",
  H3K4me3=" sampleType=Myeloidcell cost=30.5  assayType=H3K4me3")
name.value.pattern <- paste0(
  "(?P<name>[^ ]+?)",
  "=",
  "(?P<value>[^ ]+)")
namedCapture::str_match_all_named(name.value.vec, name.value.pattern)

}
