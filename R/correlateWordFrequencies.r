#' Function correlateCodeFrequencies
#' 
#' Correlates word frequencies across documents.
#' @param corpus Character vector containing one document per element.
#' @details Correlates word frequencies across documents.
#' @keywords text mining
#' @export
#' @examples
#' correlateWordFrequencies(c("<1>hello</1> world","<1>hello</1> world","<2>hallo</2>"))

correlateWordFrequencies <- function (corpus, binary = T, min = NULL, returnVector = T, plot = T) 
{
    t = vecToTDM(corpus, min = 0, plot = F)
    if (binary) 
        t = apply(t > 0, c(1, 2), as.numeric)
    a = cor(t(t))
    co2 = a
    diag(co2) = NA
    co2 = c(co2)
    names(co2) = paste(rep(colnames(a), length(rownames(a))), 
        rep(rownames(a), each = length(colnames(a))))
    co2 = sort(co2, decreasing = T)
    if (!is.null(min)) 
        co2 = co2[abs(co2) >= min]
    if (length(co2) > 10) 
        if (is.null(min)) 
            co2 = co2[c(1:10, (length(co2) - 9):length(co2))]
    if (plot & length(co2) > 2) 
        spiderplot((co2 + 1)/2, main = "Correlations of word-frequencies")
    if (returnVector) 
        return(co2)
    if (!returnVector) 
        return(co)
}
