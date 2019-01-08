#' Function af.lda
#' @keywords modeling
#' @export


af.lda <- function (data = c("Hallo Welt", "Hello World"), k = 3, method = "Gibbs", 
    nstart = 10, seed = 0) 
{
    require(topicmodels)
    data = data.frame(data)
    if (seed > 0) 
        set.seed(seed)
    d = t(vecToTDM(as.data.frame(data)[[1]], min = 0, plot = F))
    d = d[!is.na(rownames(d)) & !rowSums(d) == 0, ]
    l = LDA(d, k, method = method, control = list(nstart = nstart, 
        seed = as.list(1:nstart + as.numeric(gsub("-", "", Sys.Date()))), 
        best = T, burnin = 4000, iter = 2000, thin = 500))
    attr(l, "af.data") = data
    attr(l, "af.dtm") = d
    return(l)
}
