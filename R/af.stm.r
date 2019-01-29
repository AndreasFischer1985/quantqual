#' Function af.stm
#' 
#' Applies stm::stm to a character vector containing documents.
#' @param data Character vector containing data.
#' @param k Number of topics. If length(k)>1, kSTM will be applied to suggest an optimal number of topics.
#' @keywords modeling
#' @export


af.stm <- function (data = c("a b c d e f g h i j k l m n o p", "a b c Hello World"), 
    k = 2:10, lower.thresh = 0, lowercase = T, removestopwords = F, 
    removenumbers = F, removepunctuation = T, stem = F, language = "german", 
    wordLengths = c(0, Inf), sparselevel = 1, verbose = TRUE, 
    onlycharacter = FALSE, striphtml = F, seed1 = 0, attrData = F, 
    plot.kSTM = T, init.type = "LDA", ...) 
{
    data = data.frame(data)
    proc = stm::textProcessor(as.character(data[[1]]), metadata = data, 
        lowercase = lowercase, removestopwords = removestopwords, 
        removenumbers = removenumbers, removepunctuation = removepunctuation, 
        stem = stem, wordLengths = wordLengths, sparselevel = sparselevel, 
        language = language, verbose = verbose, onlycharacter = onlycharacter, 
        striphtml = striphtml)
    d = stm::prepDocuments(proc$documents, proc$vocab, proc$meta, 
        lower.thresh = lower.thresh)
    if (seed1 > 0) 
        set.seed(seed1)
    if (length(k) > 1) 
        k = kSTM(K = k, data = data, seed1 = seed1 - 1, plot = plot.kSTM, 
            removestopwords = removestopwords, init.type = "LDA")[[1]]
    s = stm::stm(d$documents, d$vocab, K = k, ...)
    if (attrData) {
        attr(s, "af.data") = data
        attr(s, "af.textProcessor") = proc
        attr(s, "af.prepDocuments") = d
    }
    return(s)
}
