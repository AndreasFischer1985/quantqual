% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveDevs.r
\name{saveDevs}
\alias{saveDevs}
\title{Function saveDevs}
\usage{
saveDevs(
  filename = NA,
  width = NA,
  height = NA,
  dev = png,
  units = "in",
  res = 300,
  mess = T,
  close = F,
  ...
)
}
\arguments{
\item{filename}{Character vector containing file names.}

\item{width}{Width of the devices. If NA (default), width of the devices is left unchanged.}

\item{height}{Height of the devices. If NA (default), height of the devices is left unchanged.}

\item{dev}{Graphical device (currently, pdf, win.metafile, png, bmp, jpg and tiff are supported).}

\item{units}{Parameter passed to dev.copy. Defaults to "in". Is ignored if dev==pdf.}

\item{res}{Resolution (dots per inch) of the devices. Defaults to 300. Is ignored if dev==pdf.}

\item{mess}{Logical value specifying if a message should be printed after saving the devices. Defaults to T.}

\item{close}{Logical value specifying whether to close devices after saving them.}
}
\description{
Saves each element of dev.list() to a graphical device such as pdf, png or similar.
}
\details{
Saves each element of dev.list() to a graphical device such as pdf, png or similar.
}
\examples{
saveDevs();
}
\keyword{plotting}
