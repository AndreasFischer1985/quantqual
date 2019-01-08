#' Function uncodeCode
#' 
#' Removes all codings of a certain code from a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Removes all codings of a certain code from a character vector.
#' @keywords text mining
#' @export
#' @examples
#' uncodeCode("<1>hello</1> world")

uncodeCode <- function (corpus, code = "c1", mess = T, assign = T) 
{
    for (i in 1:length(corpus)) {
        corpus[i] = gsub(paste0("<[/]?", code, ">"), "", corpus[i])
    }
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
