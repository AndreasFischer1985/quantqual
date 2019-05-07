#' Function vecToDTM
#' 
#' Document-Term matrix based on character vector.
#' @param corpus Character vector containing one document per element.
#' @details Document-Term matrix based on character vector.
#' @keywords text mining
#' @export
#' @examples
#' vecToDTM("hello world")

vecToDTM <- function (corpus = "hello,  \nworld", stopwords = NULL, lowerCase = T, 
    min = 0, minDocs = 0, sort = F, plot = F, remove_punct = T, 
    ...) 
{
    m = as.matrix(quanteda::dfm(corpus, tolower = lowerCase, 
        remove_punct = remove_punct))
    if (!is.null(stopwords)) 
        m = m[, is.na(match(colnames(m), stopwords))]
    if (sort) 
        m = m[order(rowSums(m)), ]
    m1 = NULL
    m1 = as.matrix(m[rowSums(m) > 0, ])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)
        rownames(m1) = rownames(m)[rowSums(m) > 0]
    }
    if (sum(dim(m)) != sum(dim(m1))) 
        m = m1
    if (!is.null(min)) 
        m1 = as.matrix(m1[, colSums(m) >= min])
    else if (is.null(min)) 
        m1 = as.matrix(m1[, c(1:10, (dim(m1)[2] - 9):dim(m1)[2])])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)[colSums(m) >= min]
    }
    if (sum(dim(m)) != sum(dim(m1))) 
        m = m1
    if (!is.null(minDocs)) 
        m1 = as.matrix(m1[, colSums(m1 > 0) >= minDocs])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)[colSums(m > 0) >= minDocs]
    }
    if (sum(dim(m)) != sum(dim(m1))) 
        m = m1
    if (paste(dim(m), collapse = "") != paste(dim(m1), collapse = "")) 
        warning("Dimensions of document-term-matrix reduced")
    if (plot) 
        bp = quantqual::bp(t(m1), main = "Word frequencies per document", 
            beside = F)
    return(m1)
}
