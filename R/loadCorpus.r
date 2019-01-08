#' Function loadCorpus
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


loadCorpus <- function (path = "2017-12-29 corpus.txt") 
{
    read.table(corpus, paste0(Sys.Date(), " ", deparse(substitute(corpus)), 
        ".txt"))
}
