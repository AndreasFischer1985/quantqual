#' Function af.ctm
#' 
#' Applies topicmodels::CTM to a character vector containing documents.
#' @param data Character vector containing data.
#' @param k Number of topics. If length(k)>1, kCTM will be applied to suggest an optimal number of topics.
#' @keywords modeling
#' @export


af.ctm <- function (data = c("a b c d e f g h i j k l m n o p", "a b c Hello World"), 
    k = 3, method = "VEM", seed1 = 0, control = list(nstart = 10, 
        seed = NULL, best = T, burnin = 4000, iter = 2000, thin = 500), 
    dtm = NULL, attrData = F) 
{
    if (is.null(dtm) & is.null(data)) 
        stop("Please provide either a character vector data or Document-Term-Matrix dtm")
    if (trace) 
        message(paste0("Started processing at ", Sys.time()))
    if (!is.null(data) & !is.null(stopwords)) {
        data = gsub(paste0("(\\b", paste(stopwords, collapse = "\\b|\\b"), 
            "\\b)"), "", data)
    }
    if (is.null(dtm)) {
        dtm = t(vecToTDM(as.data.frame(data)[[1]], min = 0, plot = F))
        dtm = dtm[!is.na(rownames(dtm)) & !rowSums(dtm) == 0, 
            ]
        if (trace) 
            message(paste0("DTM created at ", Sys.time()))
    }
    if (!is.null(stopwords)) {
        dtm = dtm[, is.na(match(colnames(dtm), stopwords))]
    }
    if (!is.null(control)) 
        if (is.null(control$seed)) 
            control$seed = as.list(1:control$nstart + ifelse(is.null(seed1), 
                0, seed1), control$seed)
    data = data.frame(data)
    if (seed1 > 0) 
        set.seed(seed1)
    c = topicmodels::CTM(dtm, k, method = method, control = control)
    if (attrData) {
        attr(l, "af.data") = data
        attr(l, "af.dtm") = dtm
    }
    return(c)
}
