% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/af.lda.r
\name{af.lda}
\alias{af.lda}
\title{Function af.lda}
\usage{
af.lda(data = c("a b c d e f g h i j k l m n o p", "a b c Hello World"),
  k = 2:10, alpha = NULL, delta = NULL, method = "Gibbs",
  seed1 = 0, control = list(nstart = 1, seed = NULL, alpha = NULL,
  delta = NULL, best = T, burnin = 4000, iter = 1000, thin = 500),
  dtm = NULL, stopwords = NULL, attrData = F, plot.kLDA = T,
  trace = T, lowercase = T)
}
\arguments{
\item{data}{Character vector containing data.}

\item{k}{Number of topics. If length(k)>1, kLDA will be applied to suggest an optimal number of topics.}
}
\description{
Applies topicmodels::LDA to a character vector containing documents.
}
\keyword{modeling}
