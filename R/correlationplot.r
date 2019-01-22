#' Function correlationplot
#' 
#' Plots correlations based on a data.frame.
#' @param x Numeric data.frame or matrix containing the vectors whose correlations should be displayed.
#' @param stars Logical value indicating whether significant results should be marked with stars. Defaults to T.
#' @param numbers Logical value indicating whether correlations should be plotted numerically. Defaults to T.
#' @param cex Numerical value specifying the relative size of texts to be displayed. Defaults to .6.
#' @param show.legend Logical value indicating whether a legend shoud be displayed. Defaults to F.
#' @param ... Additional arguments to be passed to psych::cor.plot.
#' @details Plots correlations based on a data.frame.
#' @keywords plotting
#' @export
#' @examples
#' set.seed(0);correlationPlot(data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100)))

correlationplot <- function (x = NULL, stars = T, numbers = T, cex = 0.6, show.legend = F, 
    ...) 
{
    if (is.null(x)) 
        x = data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
    data = x
    if (is.null(dim(data))) 
        stop("data should be a data.frame or a numeric matrix")
    if (is.null(data)) 
        data = data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
    psych::cor.plot(data, stars = stars, numbers = numbers, cex = cex, 
        show.legend = show.legend, ...)
    return(psych::corr.test(data))
}
