#' Function vecToDTM
#' 
#' Document-Term matrix based on character vector.
#' @param corpus Character vector containing one document per element.
#' @details Document-Term matrix based on character vector.
#' @keywords text mining
#' @export
#' @examples
#' vecToDTM("hello world")

vecToDTM <- function (corpus = "hello,  \nworld", lowerCase = T, min = 0, 
    sort = F, plot = F, remove_punct = T, ...) 
{
    m = quanteda::dfm(corpus, tolower = lowerCase, remove_punct = remove_punct, 
        ...)
    if (sort) 
        m = m[order(rowSums(m)), ]
    m1 = NULL
    if (!is.null(min)) 
        m1 = m[rowSums(m) >= min, ]
    if (is.null(min)) 
        m1 = m[c(1:10, (dim(m)[1] - 9):dim(m)[1]), ]
    if (is.null(dim(m1))) {
        dim(m1) = c(sum(rowSums(m) >= min), dim(m)[2])
        colnames(m1) = colnames(m)
        rownames(m1) = n[rowSums(m) >= min]
    }
    if (plot) 
        bp = bp(t(m1), main = "Word frequencies per document", 
            beside = F)
    return(m1)
}
