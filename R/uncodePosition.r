#' Function uncodeCorpus
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


uncodePosition <- function (corpus, position, mess = T, assign = T) 
{
    c2 = corpus[position[1]]
    corpus[position[1]] = paste0(substring(c2, 1, position[2]), 
        gsub("<[/]?[A-Za-z0-9]+>", "", substring(c2, position[2] + 
            1, position[3])), substring(c2, position[3] + 1, 
            nchar(c2)))
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
