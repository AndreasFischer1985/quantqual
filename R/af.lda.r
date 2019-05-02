#' Function af.lda
#' 
#' Applies topicmodels::LDA to a character vector containing documents.
#' @param data Character vector containing data.
#' @param k Number of topics. If length(k)>1, kLDA will be applied to suggest an optimal number of topics.
#' @keywords modeling
#' @export


af.lda <- function (data = c("a b c d e f g h i j k l m n o p", "a b c Hello World"), 
    k = 2:10, alpha = NULL, delta = NULL, method = "Gibbs", seed1 = 0, 
    control = list(nstart = 1, seed = NULL, alpha = NULL, delta = NULL, 
        best = T, burnin = 4000, iter = 1000, thin = 500), dtm = NULL, 
    stopwords = NULL, attrData = F, plot.kLDA = T, trace = T, 
    lowercase = T) 
{
    if (is.null(dtm) & is.null(data)) 
        stop("Please provide either a character vector data or Document-Term-Matrix dtm")
    if (trace) 
        message(paste0("Started processing at ", Sys.time()))
    if (lowercase) 
        data = tolower(data)
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
    if (!is.null(control)) {
        if (is.null(control$seed)) 
            control$seed = as.list(1:control$nstart + ifelse(is.null(seed1), 
                0, seed1))
        message(paste("seed=", control$seed))
        if (!is.null(alpha)) 
            control$alpha = alpha
        if (is.null(control$alpha)) 
            control$alpha = min(0.1, ifelse(is.null(k), 0.1, 
                50/k))
        if (!is.null(delta)) 
            control$delta = delta
        if (is.null(control$delta)) 
            control$delta = 0.1
    }
    data = data.frame(data)
    if (seed1 > 0) 
        set.seed(seed1)
    if (length(k) > 1) {
        if (trace) 
            message("Start search for optimal k")
        k = kLDA(K = k, dtm = dtm, method = method, control = control, 
            seed1 = seed1 - 1, plot = plot.kLDA)[[1]]
        if (trace) 
            message(paste0("k=", k))
    }
    l = topicmodels::LDA(dtm, k, method = method, control = control)
    if (attrData) {
        attr(l, "af.data") = data
        attr(l, "af.dtm") = dtm
    }
    if (trace) 
        message(Sys.time())
    return(l)
}
