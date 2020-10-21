#' Function clusterWordFrequencies
#' 
#' Cluster analysis of words based on a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Cluster analysis of words based on a character vector.
#' @keywords text mining
#' @export
#' @examples
#' clusterWordFrequencies(c("<1>hello</1> world"),c("<1>hello</1> world","<2>hallo</2>"))

clusterWordFrequencies <- function (corpus, cor = F, binary = T, min = 0, plot = T) 
{
    data = t(vecToTDM(corpus, min = 0, plot = F))
    if (binary) 
        t = apply(data > 0, c(1, 2), as.numeric)
    docs = apply(data > 0, 2, sum)
    if (is.null(min)) 
        min = 1
    dat = data[, docs >= min]
    if (cor) 
        cluster = (hclust(dist(cor(dat))))
    if (!cor) 
        cluster = (hclust(dist(t(dat))))
    if (plot) 
        plot(cluster)
}
