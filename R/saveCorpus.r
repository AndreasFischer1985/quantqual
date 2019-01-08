#' Function saveCorpus
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


saveCorpus <- function (corpus) 
{
    write.table(corpus, paste0(Sys.Date(), deparse(substitute(corpus)), 
        ".txt"))
}
