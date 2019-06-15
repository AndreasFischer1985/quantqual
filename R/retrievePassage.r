#' Function retrievePassage
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


retrievePassage <- function (corpus, passage, context = F) 
{
    if (context) 
        passage = paste0("[^.;?!]*", passage, "(.*?)([.;?!]+|$)")
    l = stringr::str_locate_all(corpus, passage)
    m = stringr::str_match_all(corpus, passage)
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
    d = data.frame(text = m1, document = i1, from = l1, to = l2)
    if (dim(d)[1] > 0) 
        rownames(d) = 1:length(m1)
    return(d)
}
