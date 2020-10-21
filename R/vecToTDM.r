#' Function vecToTDM
#' 
#' Term-Document matrix based on character vector.
#' @param corpus Character vector containing one document per element.
#' @details Term-Document matrix based on character vector.
#' @keywords text mining
#' @export
#' @examples
#' vecToTDM("hello world")

vecToTDM <- function (corpus = "hello,  \nworld", stopwords = NULL, lowerCase = T, 
    min = 0, minDocs = 0, sort = F, plot = F) 
{
    if (lowerCase) 
        corpus = tolower(corpus)
    s = list()
    n = character(0)
    for (i in 1:length(corpus)) {
        dtm = gsub("\\W", " ", corpus[i])
        s[i] = strsplit(dtm, " ")
        s[[i]] = s[[i]][s[[i]] != ""]
        s[[i]] = sort(table(s[[i]]), decreasing = T)
        n = c(n, names(s[[i]]))
    }
    n = levels(as.factor(n))
    m = matrix(nrow = length(n), ncol = length(corpus))
    rownames(m) = n
    colnames(m) = paste0("d", 1:length(corpus))
    for (i in 1:length(n)) for (j in 1:length(corpus)) {
        m[i, j] = s[[j]][n[i]]
    }
    m[is.na(m)] = 0
    m = data.frame(m)
    if (!is.null(stopwords)) 
        m = m[is.na(match(rownames(m), stopwords)), ]
    if (sort) 
        m = m[order(rowSums(m)), ]
    m1 = NULL
    m1 = as.matrix(m[, colSums(m) > 0])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)[colSums(m) > 0]
        rownames(m1) = rownames(m)
    }
    if (sum(dim(m)) != sum(dim(m1))) 
        m = m1
    if (!is.null(min)) 
        m1 = as.matrix(m[rowSums(m) >= min, ])
    if (is.null(min)) 
        m1 = as.matrix(m[c(1:10, (dim(m)[1] - 9):dim(m)[1]), 
            ])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)
        rownames(m1) = rownames(m)[rowSums(m > 0) >= minDocs]
    }
    if (sum(dim(m)) != sum(dim(m1))) 
        m = m1
    if (!is.null(minDocs)) 
        m1 = as.matrix(m1[rowSums(m1 > 0) >= minDocs, ])
    if (is.null(colnames(m1))) {
        colnames(m1) = colnames(m)
        rownames(m1) = rownames(m)[rowSums(m > 0) >= minDocs]
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
