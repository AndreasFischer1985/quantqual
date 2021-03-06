% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnets.r
\name{nnets}
\alias{nnets}
\title{Function nnets}
\usage{
nnets(data.train, output = NULL, rep.nnet = 10, seed = 0, plot = F,
  size = 3, decay = 0, linout = T, trace = F, ...)
}
\arguments{
\item{data.train}{data.frame containing training data.}

\item{output}{index or name of criterion in training data.}
}
\description{
Fits multiple initializations of nnet and returns a sorted list of nnet-objects.
}
\examples{
 
}
\keyword{modeling}
