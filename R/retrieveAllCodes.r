#' Function retrieveAllCodes
#' 
#' Retrieves all codes in a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Retrieves all codes in a character vector.
#' @keywords text mining
#' @export
#' @examples
#' retrieveAllCodings("<1>hello</1> world")

retrieveAllCodes <- function (corpus) 
{
    coding = paste0("<[A-Za-z0-9]+>")
    m = stringr::str_match_all(corpus, coding)
    m1 = character(0)
    for (i in 1:length(corpus)) {
        m1 = c(m1, m[[i]][, 1])
    }
    return(levels(as.factor(m1)))
}
