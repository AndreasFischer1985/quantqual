#' Function af.ctm
#' @keywords modeling
#' @export


af.ctm <- function (data = c("Hallo Welt", "Hello World"), k = 3, method = "VEM", 
    nstart = 10, seed = 0) 
{
    library(topicmodels)
    data = data.frame(data)
    if (seed > 0) 
        set.seed(seed)
    d = t(vecToTDM(as.data.frame(data)[[1]], min = 0, plot = F))
    d = d[!is.na(rownames(d)) & !rowSums(d) == 0, ]
    c = CTM(d, k, method = method, control = list(nstart = nstart, 
        seed = as.list(1:nstart + as.numeric(gsub("-", "", Sys.Date()))), 
        best = T))
    attr(c, "af.data") = data
    attr(c, "af.dtm") = d
    return(c)
}
