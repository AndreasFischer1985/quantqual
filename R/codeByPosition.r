#' Function codeByPosition
#' 
#' Adds code tags to certain positions of a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Adds code tags to certain positions of a character vector.
#' @keywords text mining
#' @export
#' @examples
#' codeByPosition("hello world hello world hello world hello world",data.frame(c(1,1,5)),"code1")

codeByPosition <- function (corpus, position, coding, mess = T, assign = F) 
{
    position = data.frame(position)
    if (length(coding) == 1) 
        coding = rep(coding, dim(position)[2])
    for (i in 1:length(corpus)) for (j in 1:dim(position)[2]) {
        if (position[1, j] == i) {
            passage = substr(corpus[i], position[2, j], position[3, 
                j])
            corpus[i] = gsub(passage, paste0("<", coding[j], 
                ">", passage, "</", coding[j], ">"), corpus[i])
        }
    }
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
