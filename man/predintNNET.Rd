% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predintNNET.r
\name{predintNNET}
\alias{predintNNET}
\title{Function predintNNET}
\usage{
predintNNET(nnet = NULL, xTrain = NULL, yTrain = NULL,
  xTest = NULL, yTest = NULL, alpha = 0.05, lambda = 0.5,
  funName = "sigmoid", fun2Name = "linear",
  main = "Nonlinear Regression",
  sub = "shaded area represent prediction interval.",
  xlab = "Predictor", ylab = "Criterion", plot = T, col1 = rgb(0,
  0, 0, 0.8), col2 = rgb(0, 0, 1), col3 = rgb(0, 0, 1, 0.2),
  pch = 16, lwd = 2, cex.sub = 0.7, ...)
}
\arguments{
\item{nnet}{A nnet object.}

\item{xTrain}{data.frame with one or more numeric vectors.}

\item{yTrain}{numeric vector.}

\item{xTest}{data.frame with one or more numeric vectors.  If NULL (default) equals xTrain.}

\item{yTest}{numeric vector. If NULL (default) equals xTrain.}

\item{alpha}{Alpha level. Defaults to 0.05.}

\item{lambda}{lambda. Defaults to 0.5.}

\item{funName}{Activation function for the hidden-layer of the nnet-object ("sigmoid","tanh","linear"). Defaults to "sigmoid".}

\item{fun2Name}{Activation function for the output-layer of the nnet-object ("sigmoid","tanh","linear"). Defaults to "sigmoid".}

\item{color1}{Color of points in the scattergram. Defaults to rgb(0,0,0,.7).}

\item{color2}{Color of the regression line. Defaults to rgb(0,0,1).}

\item{color3}{Color of the prediction interval. Defaults to rgb(0,0,1,.2).}

\item{color1}{Color of points in the scattergram. Defaults to rgb(0,0,0,.7).}

\item{color2}{Color of the regression line. Defaults to rgb(0,0,1).}

\item{color3}{Color of the prediction interval. Defaults to rgb(0,0,1,.2).}
}
\description{
Returns and plots prediction interval for a neural network.
}
\details{
Returns and plots prediction interval for a neural network. This function is a modification of nnetpredint::nnetPredInt version 1.2. The code was modified by Andreas Fischer to allow for prediction intervals of nnet-objetcs with linear output units (i.e., parameter linout=T in nnet::nnet). The nnetpredint-package (https://cran.r-project.org/package=nnetpredint) was written by Xichen Ding <rockingdingo at gmail.com> under GPL-License (>= 2). There is no waranty for the work whatsoever.
}
\examples{
set.seed(0);
d=data.frame(x=scale(rnorm(100)*10+1:100),y=scale(rnorm(100)*10+1:100),z=scale(rnorm(100)*10+1:100));
d=d[order(d[,"x"]),]
dev.new();
n=nnet::nnet(y ~ ., data=d, size=2, rang=0.1, decay=5e-4, maxit=500, linout=T)
p=predintNNET(nnet=n, xTrain=d[c("x","z")], yTrain=d["y"], alpha = 0.05, funName = "sigmoid", fun2Name = "linear")
dev.new();
n=nnet::nnet(y ~ x, data=d, size=2, rang=0.1, decay=5e-4, maxit=500, linout=T)
p=predintNNET(nnet=n, xTrain=d[c("x")], yTrain=d["y"], alpha = 0.05, funName = "sigmoid", fun2Name = "linear")
}
\keyword{modeling}
