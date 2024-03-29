% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vecToTDM.r
\name{vecToTDM}
\alias{vecToTDM}
\title{Function vecToTDM}
\usage{
vecToTDM(
  corpus = "hello,  \\nworld",
  stopwords = NULL,
  lowerCase = T,
  min = 0,
  minDocs = 0,
  sort = F,
  plot = F
)
}
\arguments{
\item{corpus}{Character vector containing one document per element.}
}
\description{
Term-Document matrix based on character vector.
}
\details{
Term-Document matrix based on character vector.
}
\examples{
vecToTDM("hello world")
}
\keyword{mining}
\keyword{text}
