#' Function compareDocs
#' 
#' Plots difference between two documents as packed bubble chart.
#' @param a Character value.
#' @param b Character value.
#' @param relative Logical value. Defaults to T.
#' @param main Character vector with one element containing the plot's title. Defaults to NULL
#' @param show.text Logical value indicating whether bubbles should be labelled with names(vec). Defaults to T.
#' @param beak.names Logical value indicating whether line breaks should be added after each whitespace. Defaults to F.
#' @param beak.names Logical value indicating whether values of vec should be added to vec's names before labelling the bubbles.
#' @param cex Numeric value indicating the text size. Defaults to .8.
#' @param a Numeric value multiplied with normalizd vec. Defaults to 90.
#' @param b Numeric value added to normalized vec*a. Defaults to 10.
#' @param col Character vector indicating the bubbles' color. If NULL (default) the rainbow palette is applied to vec's ranks.
#' @details Plots vector as packed bubble chart. Returns coordinates and radius of each bubble
#' @keywords plotting
#' @export
#' @examples
#' compareDocs("This is document 1","this is another document")

compareDocs <- function (a = NULL, b = NULL, legendtext = c("a", "b"), relative = T, 
    ...) 
{
    if (is.null(a) & is.null(b)) {
        a = "a a b d c c"
        b = "a b b b b c"
    }
    if (is.null(a) | is.null(b)) 
        stop("Please provide two character elements a and b")
    a = paste(a, collapse = " ")
    b = paste(b, collapse = " ")
    c = vecToTDM(c(a, b))
    common.terms = subset(c, c[, 1] > 0 & c[, 2] > 0)
    if (relative) 
        common.terms = apply(common.terms, 2, function(x) x/sum(x))
    common.terms = common.terms/colSums(common.terms)
    diff = abs(common.terms[, 1] - common.terms[, 2])
    common.terms = cbind(common.terms, diff)
    common.terms = common.terms[order(common.terms[, 3], decreasing = T), 
        ]
    com = common.terms[dim(common.terms)[1]:1, ]
    com = com[com[, 3] > 0, ]
    col = character(length(com[, 1]))
    diff1 = com[, 3][com[, 2] > com[, 1]]
    col[com[, 2] > com[, 1]] = cols(length(diff1), "lightblue", 
        "blue")[rank(diff1)]
    diff1 = com[, 3][com[, 2] < com[, 1]]
    col[com[, 2] < com[, 1]] = cols(length(diff1), "orange", 
        "red")[rank(diff1)]
    col[com[, 2] == com[, 1]] = "white"
    if (sd(com[, 3]) > 0) {
        packedBubbleChart(round(com[, 3], 2), col = col, ...)
        legend("topright", legendtext, fill = c("red", "blue"))
    }
    else warning("No variance in common terms")
    return(com)
}
