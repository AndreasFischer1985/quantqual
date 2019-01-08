#' Function uncodePassage
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


uncodePassage <- function (corpus, passage, mess = T, assign = T) 
{
    for (i in 1:length(corpus)) {
        corpus[i] = gsub(paste0("<[A-Za-z0-9]+>", passage, "</[A-Za-z0-9]+>"), 
            passage, corpus[i])
    }
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
