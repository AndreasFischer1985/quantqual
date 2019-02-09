#' Function dotplot
#' 
#' Plots a modified dotchart.
#' @param x Numeric vector.
#' @param relative Logical value. Defaults to T.
#' @param vertLine Logical value specifying whether to plot a vertical line at x=0. Defaults to T.
#' @param horizLines Logical value specifying whether to plot horizontal lines between each point and the y axis. Defaults to T.
#' @param max Numeric value indicating the maximum number of frequencies to be displayed. Defaults to 40.
#' @param main Character value specifying the plot's title. Defaults to "Comparison of Word Frequencies"
#' @param pch Numeric element speccifying the symbol to represent points. Defaults to 16
#' @param cex Numeric value indicating the text size. Defaults to .7.
#' @details Plots vector as modified dotchart.
#' @keywords plotting
#' @export
#' @examples
#' dotplot(rnorm(100))

dotplot <- function (x = NULL, relative = T, vertLine = T, horizLines = T, 
    max = 40, main = NULL, pch = 16, cex = 0.7, ...) 
{
    if (is.null(x)) {
        x = rnorm(100)
        names(x) = paste0("Var", 1:100)
    }
    x = sort(x)
    if (length(x) > max) {
        x = x[c(1:floor(max/2), (length(x) - floor(max/2)):length(x))]
        warning(paste("Only", max, "values are displayed"))
    }
    dotchart(x, pch = pch, main = main, cex = cex, ...)
    if (horizLines) 
        segments(x0 = 0, y0 = 1:length(x), x1 = x, y1 = 1:length(x))
    if (vertLine) 
        abline(v = 0)
}
