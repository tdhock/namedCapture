\name{namedCapture.engine}
\alias{namedCapture.engine}
\title{namedCapture engine}
\description{G\code{e}t curr\code{e}nt r\code{e}g\code{e}x \code{e}ngin\code{e} us\code{e}d by str_match_nam\code{e}d and
str_match_all_nam\code{e}d (if no argum\code{e}nts), or s\code{e}t th\code{e} curr\code{e}nt \code{e}ngin\code{e}
(if an argum\code{e}nt is giv\code{e}n). Th\code{e} "nam\code{e}dCaptur\code{e}.\code{e}ngin\code{e}" option is
us\code{e}d, and can b\code{e} s\code{e}t by th\code{e} us\code{e}r,
i.\code{e}. options(nam\code{e}dCaptur\code{e}.\code{e}ngin\code{e}=\code{e}) is th\code{e} sam\code{e} as
nam\code{e}dCaptur\code{e}.\code{e}ngin\code{e}(\code{e}). RE2 is us\code{e}d by d\code{e}fault if th\code{e} r\code{e}2r packag\code{e}
is availabl\code{e}.}
\usage{namedCapture.engine(e)}
\arguments{
  \item{e}{r\code{e}g\code{e}x \code{e}ngin\code{e}, \code{e}ith\code{e}r "PCRE" or "RE2" -- RE2 can only b\code{e} us\code{e}d if
th\code{e} r\code{e}2r packag\code{e} is availabl\code{e}.}
}



\author{Toby Dylan Hocking}




\examples{

library(namedCapture)
old.engine <- namedCapture.engine()
namedCapture.engine("PCRE")
namedCapture.engine()
namedCapture.engine("RE2")
namedCapture.engine()
namedCapture.engine(old.engine)
namedCapture.engine()

}
