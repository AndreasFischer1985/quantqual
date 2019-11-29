#' Function compareDocs
#' 
#' Plots difference between two documents as a modified dotchart.
#' @param a Character value.
#' @param b Character value.
#' @param relative Logical value. Defaults to T.
#' @param vertLine Logical value specifying whether to plot a vertical line at x=0. Defaults to T.
#' @param horizLines Logical value specifying whether to plot horizontal lines between each point and the y axis. Defaults to T.
#' @param max Numeric value indicating the maximum number of word frequencies to be displayed. Defaults to 40.
#' @param main Character value specifying the plot's title. Defaults to "Comparison of Word Frequencies"
#' @param pch Numeric element speccifying the symbol to represent points. Defaults to 16
#' @param cex Numeric value indicating the text size. Defaults to .7.
#' @details Plots difference between two documents as a modified dotchart. Returns matrix with relative frequencies and differences between documents a and b.
#' @keywords plotting
#' @export
#' @examples
#' compareDocs("This is document 1","this is another document")

compareDocs <- function (a = NULL, b = NULL, relative = T, vertLine = T, horizLines = T, 
    max = 40, main = "Comparison of Word Frequencies", pch = 16, 
    cex = 0.7, ...) 
{
    if (is.null(a) & is.null(b)) {
        a = "a a b d c c"
        b = "a b b b b c"
    }
    if (is.null(a) | is.null(b)) 
        stop("Please provide two character elements a and b")
    a = paste(a, collapse = " ")
    b = paste(b, collapse = " ")
    common.terms = quantqual::vecToTDM(c(a, b))
    if (relative) 
        common.terms = apply(common.terms, 2, function(x) x/sum(x))
    common.terms = common.terms/colSums(common.terms)
    diff = (common.terms[, 1] - common.terms[, 2])
    common.terms = cbind(common.terms, diff)
    common.terms = common.terms[order(common.terms[, 3], decreasing = F), 
        ]
    com = common.terms
    if (dim(com)[1] > max) {
        com = com[c(1:floor(max/2), (dim(com)[1] - floor(max/2)):dim(com)[1]), 
            ]
        warning(paste("Only", max, "differences are displayed"))
    }
    dotchart(com[, 3], pch = pch, main = main, cex = cex, ...)
    if (horizLines) 
        segments(x0 = 0, y0 = 1:dim(com)[1], x1 = round(com[, 
            3], 2), y1 = 1:dim(com)[1])
    if (vertLine) 
        abline(v = 0)
    invisible(common.terms)
}
