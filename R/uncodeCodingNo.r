#' Function uncodeCorpus
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


uncodeCodingNo <- function (corpus, codingNo = 1, mess = T, assign = T) 
{
    r = retrieveAllCodings(corpus, context = F)
    doc = r[codingNo, 2]
    corpus[doc] = paste0(substring(corpus[doc], 1, r[codingNo, 
        3] - 1), gsub("<(.*?)>", "", r[codingNo, 1]), substring(corpus[doc], 
        r[codingNo, 4] + 1, nchar(corpus[doc])))
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
}
