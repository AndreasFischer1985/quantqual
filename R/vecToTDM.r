#' Function vecToTDM
#' 
#' Term-Document matrix based on character vector.
#' @param corpus Character vector containing one document per element.
#' @details Term-Document matrix based on character vector.
#' @keywords text mining
#' @export
#' @examples
#' vetToDTM("hello world")

vecToTDM <- function (corpus = "hello,  \nworld", lowerCase = T, min = 0, 
    sort = F, plot = T) 
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
    if (sort) 
        for (i in 1:dim(m)[1]) for (j in 1:dim(m)[1]) if (i > 
            j & sum(m[i, ]) < sum(m[j, ])) {
            h = m[i, ]
            m[i, ] = m[j, ]
            m[j, ] = h
        }
    m = as.matrix(m)
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
