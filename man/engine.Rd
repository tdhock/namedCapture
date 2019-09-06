\name{engine}
\alias{engine}
\title{engine}
\description{Get current regex engine used by \code{\link{str_match_named}} and
\code{\link{str_match_all_named}}. RE2 is used by default if the re2r package is
available; otherwise, PCRE is used by default. The user can set
\code{options(engine="PCRE")} to use PCRE even when
RE2 is available.}
\usage{engine()}



\author{Toby Dylan Hocking}




\examples{

namedCapture::engine()
old.opt <- options(engine="PCRE")
namedCapture::engine()
options(old.opt)

}
