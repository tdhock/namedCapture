\name{namedCapture.engine}
\alias{namedCapture.engine}
\title{namedCapture engine}
\description{Get current regex engine used by \code{\link{str_match_named}} and
\code{\link{str_match_all_named}}. RE2 is used by default if the re2r package is
available; otherwise, PCRE is used by default. The user can set
\code{options(namedCapture.engine="PCRE")} to use PCRE even when
RE2 is available.}
\usage{namedCapture.engine()}



\author{Toby Dylan Hocking}




\examples{

namedCapture::namedCapture.engine()
old.opt <- options(namedCapture.engine="PCRE")
namedCapture::namedCapture.engine()
options(old.opt)

}
