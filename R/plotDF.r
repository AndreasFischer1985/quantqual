#' Function plotDF
#' 
#' Plots each vector of a data.frame.
#' @param data Numeric data.frame.
#' @param col Character vector specifying each plot's color. If NULL (default) the rainbow palette is applied.
#' @details Plots each vector of a data.frame either as histogram or as barplot.
#' @keywords plotting
#' @export
#' @examples
#' plotDF()

plotDF <- function (data = NULL, col = NULL, ...) 
{
    if (is.null(data)) 
        data = data.frame(x = rnorm(100), y = rnorm(100), z = as.factor(round(rnorm(100))))
    if (is.null(dim(data))) 
        stop("data should be a data.frame or a numeric matrix")
    if (is.null(col)) 
        col = rainbow(dim(data)[2])
    par(mfrow = c(ceiling(sqrt(dim(data)[2])), ceiling(sqrt(dim(data)[2]))))
    for (i in 1:dim(data)[2]) if (is.numeric(data[, i])) {
        hist(data[, i], xlab = colnames(data)[i], main = colnames(data)[i], 
            col = col[i], ...)
    }
    else {
        quantqual::bp(c(table(data[, i])), main = colnames(data)[i], 
            col = col[i], ...)
    }
}
