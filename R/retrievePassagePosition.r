#' Function retrievePassagePosition
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


retrievePassagePosition <- function (corpus, passage, context = F) 
{
    library(stringr)
    if (context) 
        passage = paste0("[^.;?!]*", passage, "(.*?)([.;?!]+|$)")
    l = str_locate_all(corpus, passage)
    m = str_match_all(corpus, passage)
    m1 = character(0)
    l1 = numeric(0)
    l2 = numeric(0)
    i1 = numeric(0)
    for (i in 1:length(corpus)) {
        m1 = c(m1, m[[i]][, 1])
        l1 = c(l1, l[[i]][, 1])
        l2 = c(l2, l[[i]][, 2])
        i1 = c(i1, rep(i, length(m[[i]][, 1])))
    }
    d = data.frame(document = i1, from = l1, to = l2)
    if (dim(d)[1] > 0) 
        rownames(d) = make.names(m1, unique = T)
    return(d)
}
