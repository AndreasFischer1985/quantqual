#' Function uncodeCorpus
#' 
#' Removes all codings from a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Removes all codings from a character vector.
#' @keywords text mining
#' @export
#' @examples
#' uncodeCorpus("<1>hello</1> world")

uncodeCorpus <- function (corpus, mess = T, assign = T, tolower = F) 
{
    for (i in 1:length(corpus)) {
        corpus[i] = gsub("<[/]?[A-Za-z0-9]+>", "", corpus[i])
    }
    if (tolower) 
        corpus = tolower(corpus)
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
