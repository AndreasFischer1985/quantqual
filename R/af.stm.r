#' Function af.stm
#' @keywords modeling
#' @export


af.stm <- function (data = c("Hallo Welt", "Hello World"), K = 3, lower.thresh = 0, 
    removestopwords = F, ...) 
{
    library(stm)
    data = data.frame(data)
    proc = textProcessor(as.character(data[[1]]), metadata = data, 
        lowercase = T, removestopwords = removestopwords, removenumbers = F, 
        removepunctuation = T, stem = F, wordLengths = c(0, Inf), 
        sparselevel = 1, language = "german", verbose = TRUE, 
        onlycharacter = FALSE, striphtml = F)
    d = prepDocuments(proc$documents, proc$vocab, proc$meta, 
        lower.thresh = lower.thresh)
    s = stm(d$documents, d$vocab, K = K, ...)
    attr(s, "af.data") = data
    attr(s, "af.textProcessor") = proc
    attr(s, "af.prepDocuments") = d
    return(s)
}
