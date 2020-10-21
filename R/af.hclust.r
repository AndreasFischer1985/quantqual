#' Function af.hclust
#' 
#' Cluster analysis of data.frame.
#' @param data Numeric data.frame.
#' @details Cluster analysis of data.frame.
#' @keywords modeling
#' @export
#' @examples
#' af.hclust(data.frame(x=rnorm(100),y=rnorm(100)+scale(1:100),z=rnorm(100)+scale(1:100)));

af.hclust <- function (data, dist.function = function(x) as.dist(1 - cor(x)), 
    method = "complete", k = NULL, plot = T) 
{
    hclustering = hclust(dist.function(data), method = method)
    if (plot) 
        plot(hclustering, main = "hierarchical cluster analysis", 
            cex = 0.6)
    clusters = cutree(hclustering, k = 1)
    if (!is.null(k)) 
        if (k > 0) 
            clusters = cutree(hclustering, k = k)
    hclustering[["clusters"]] = clusters
    return(hclustering)
}
