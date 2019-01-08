#' Function clusterCodeFrequencies
#' 
#' Cluster analysis of codes based on a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Cluster analysis of codes based on a character vector.
#' @keywords text mining
#' @export
#' @examples
#' clusterCodeFrequencies(c("<1>hello</1> world"),c("<1>hello</1> world","<2>hallo</2>"))

clusterCodeFrequencies <- function (corpus, cor = F, binary = T, min = 0, plot = T) 
{
    data = retrieveCodeFrequencies(corpus, plot = F, binary = binary)
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
