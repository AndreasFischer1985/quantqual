#' Function kSTM
#' @keywords modeling
#' @export


kSTM <- function (data, K = c(2:10), init.type = "LDA", removestopwords = F, 
    lower.thresh = 0, seed1 = 0, plot = T, ...) 
{
    if (!is.null(list(...)[["k"]])) 
        stop("Please specify K instad of k!")
    d1 = NULL
    k = NULL
    k1 = NULL
    if (seed1 > 0) 
        set.seed(seed1)
    if (!is.null(init.type)) {
        normalize = function(x) return((x - min(x))/(max(x) - 
            min(x)))
        data = data.frame(data)
        proc = stm::textProcessor(as.character(data[[1]]), metadata = data, 
            lowercase = T, removestopwords = removestopwords, 
            removenumbers = F, removepunctuation = T, stem = F, 
            wordLengths = c(0, Inf), sparselevel = 1, language = "german", 
            verbose = TRUE, onlycharacter = FALSE, striphtml = F)
        d = stm::prepDocuments(proc$documents, proc$vocab, proc$meta, 
            lower.thresh = lower.thresh)
        k1 = stm::searchK(proc$documents, proc$vocab, K)
        d1 = t(scale(k1$results[, -c(1, 6, 7)]))
        d1 = rbind(d1, t(scale(normalize(k1$results[, 2]) * normalize(k1$results[, 
            3]))))
        k = K[which.max(normalize(k1$results[, 2]) * normalize(k1$results[, 
            3]))]
        rownames(d1)[rownames(d1) == ""] = "exclus*semcoh"
        colnames(d1) = k1$results[, 1]
        if (plot) 
            plotMAT(d1, cumsum = F, xlab = "K", ylab = "value (scaled)", 
                main = "STM Model quality based on different topics")
    }
    return(list(k = k, Results = k1))
}
