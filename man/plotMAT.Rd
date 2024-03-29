% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMAT.r
\name{plotMAT}
\alias{plotMAT}
\title{Function plotMAT}
\usage{
plotMAT(
  matrix = NULL,
  main = NULL,
  xlab = "",
  ylab = NULL,
  lwd = 2,
  lty = 1,
  pch = NULL,
  type = "l",
  xlim = NULL,
  ylim = NULL,
  xlim.factor = 1.5,
  las = 1,
  srt = 45,
  cumsum = F,
  show.legend = F,
  add = F,
  add.shadow = F,
  grid = T,
  col = NULL,
  cex = 0.7,
  cex.axis1 = 0.5,
  cex.axis2 = 0.7,
  frame = 1,
  manual.addon = NULL
)
}
\arguments{
\item{matrix}{Numeric matrix containing the values to be displayed.}

\item{main}{Character value representing the title. Defaults to NULL.}

\item{xlab}{Character value representing the x-axis label. If NULL it's set to "time". Defaults to ""}

\item{ylab}{Character value representing the y-axis label. If NULL (default) it's set to "cumulated sum" if cumsum==T, and to "value" otherwise.}

\item{lwd}{Numeric vector specifying line width. Defaults to 2.}

\item{lty}{Numeric vector specifying line type. Defaults to 1.}

\item{pch}{Numeric vector specifying points type. Defaults to NULL.}

\item{type}{Numeric vector specifying plot-type of lines. Defaults to "l".}

\item{xlim}{Numeric vector with two elements. If NULL (default) xlim is detemined automatically.}

\item{ylim}{Numeric vector with two elements. If NULL (default) ylim is detemined automatically.}

\item{xlim.factor}{Numeric value for adding extra space to the right of the plot. Defaults to 1.}

\item{las}{Numeric value specifying the rotation of the y-axis (0 for 90 percent rotation, 1 for 0 percent rotation). Defaults to 1.}

\item{srt}{Numeric value specifying the rotation of the x-axis (between 0 and 360 degrees). Defaults to 45.}

\item{cumsum}{Logical value indicating whether the cumsum of each row in the matrix of each row should be plotted. Defaults to F.}

\item{show.legend}{Logical value indicating whether a legend should be drawn instead of texts. Defaults to T.}

\item{add}{Logical value indicating whether to draw lines to an existing plot. Defaults to F.}

\item{add.shadow}{Logical value indicating whether lines should be surrounded ba a black line. Defaults to F.}

\item{grid}{Logical value indicating whether a grid should be drawn. Defaults to T.}

\item{col}{Vector containing each line's color. If NULL (default) colors are generated based on the rainbow-palette.}

\item{cex}{Relative size of legend font. Defaults to .7.}

\item{cex.axis1}{Relative size of x-axis font. Defaults to .7.}

\item{cex.axis2}{Relative size of y-axis font. Defaults to .7.}

\item{frame}{Relative size of invisible frame around fonts. Defaults to 1.}

\item{manual.addon}{Numeric vector containing verical adjustments.}
}
\description{
Plots a numeric matrix as one line per row.
}
\details{
Plots a numeric matrix as one line per row. By default cumsum of each row is plotted.
}
\examples{
plotMAT()
}
\keyword{plotting}
