#' Function codeByPassage
#' 
#' Adds code tags to certain passages of a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Adds code tags to certain passages of a character vector.
#' @keywords text mining
#' @export
#' @examples
#' codeByPassage("hello world hello world hello world hello world","hello","code1")

codeByPassage <- function (corpus, passage, coding, context = 0, mess = T, assign = T, 
    subcorpus = NULL) 
{
    library(stringr)
    context = as.numeric(context)
    if (context == 0) {
        passage = paste0("\\b", passage, "\\b")
    }
    if (context == 1) {
        passage = paste0("[^.;?!]*\\b", passage, "\\b(.*?)([.;?!]+|$)")
    }
    if (context == 2) {
        passage = paste0("(.*?)\\b", passage, "\\b(.*?)")
    }
    if (is.null(subcorpus)) 
        seq = 1:length(corpus)
    if (!is.null(subcorpus)) 
        seq = subcorpus
    for (i in seq) for (j in 1:length(passage)) if (!is.na(str_match(corpus, 
        passage[j])[i, 1])) 
        corpus[i] = gsub(passage[j], paste0("<", coding[j], ">", 
            str_match(corpus, passage[j])[i, 1], "</", coding[j], 
            ">"), corpus[i])
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
