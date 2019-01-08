#' Function correlateCodeFrequencies
#' 
#' Correlates code frequencies across documents.
#' @param corpus Character vector containing one document per element.
#' @details Correlates code frequencies across documents.
#' @keywords text mining
#' @export
#' @examples
#' correlateCodeFrequencies(c("<1>hello</1> world","<1>hello</1> world","<2>hallo</2>"))

correlateCodeFrequencies <- function (corpus, binary = T, min = NULL, returnVector = T, plot = T) 
{
    fr = retrieveCodeFrequencies(corpus = corpus, byString = T, 
        binary = binary, plot = F)
    co = cor(fr)
    co2 = co
    diag(co2) = NA
    co2 = c(co2)
    names(co2) = paste(rep(colnames(co), length(rownames(co))), 
        rep(rownames(co), each = length(colnames(co))))
    co2 = sort(co2, decreasing = T)
    if (!is.null(min)) 
        co2 = co2[abs(co2) >= min]
    if (length(co2) > 10) 
        if (is.null(min)) 
            co2 = co2[c(1:10, (length(co2) - 9):length(co2))]
    if (plot & length(co2) > 2) 
        spiderplot((co2 + 1)/2, main = "Correlations of code-frequencies")
    if (returnVector) 
        return(co2)
    if (!returnVector) 
        return(co)
}
