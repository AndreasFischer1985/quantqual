% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mar.r
\name{mar}
\alias{mar}
\title{Function mar}
\usage{
mar(
  mar = c(5.1, 4.1, 4.1, 2.1),
  mar1 = c(1, 1, 1, 1),
  mar0 = c(0, 0, 0, 0),
  labels1 = NULL,
  labels2 = NULL,
  labels3 = NULL,
  labels4 = NULL,
  labelFactor = 3,
  set = T
)
}
\arguments{
\item{mar}{Numeric vector containing default margins. Defaults to c(5.1, 4.1, 4.1, 2.1).}

\item{mar1}{Numeric vector to be multiplied with mar. Defaults to c(1,1,1,1).}

\item{mar0}{Numeric vector to be added to the product of mar and mar1. Defaults to c(0,0,0,0).}

\item{labels1}{Character vector containing labels for axis 1 (bottom).}

\item{labels2}{Character vector containing labels for axis 2 (left).}

\item{labels3}{Character vector containing labels for axis 3 (up).}

\item{labels4}{Character vector containing labels for axis 4 (right).}

\item{set}{Logical value specifying whether to set the margins after determining them. Defaults to T}
}
\description{
Returns margins of plots given numeric vectors to add and/or multiply, and/or a set of labels for each axis.
}
\details{
Returns margins of plots given numeric vectors to add and/or multiply, and/or a set of labels for each axis.
}
\examples{
mar();
}
\keyword{plotting}
