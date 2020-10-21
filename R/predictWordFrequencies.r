#' Function predictCodeFrequencies
#' 
#' Correlates word frequencies across documents with a numeric vector.
#' @param corpus Character vector containing one document per element.
#' @details Correlates word frequencies across documents with a numeric vector.
#' @keywords text mining
#' @export
#' @examples
#' correlateWordFrequencies(c("<1>hello</1> world"),c("<1>hello</1> world","<2>hallo</2>"))

predictWordFrequencies <- function (corpus, predictor, binary = T, min = NULL, mostFrequent = 10, 
    plot = T) 
{
    t = t(vecToTDM(corpus, plot = F))
    if (binary) 
        t = apply(t > 0, c(1, 2), as.numeric)
    a = cor(data.frame(predictor, t), use = "pairwise.complete")[-1, 
        1]
    b = numeric(0)
    if (mostFrequent > 0) {
        i = 0
        while (sum(colSums(t, na.rm = T) > i, na.rm = T) > mostFrequent) i = i + 
            1
        if (sum(colSums(t, na.rm = T) > i, na.rm = T) != dim(t)[2]) {
            message(paste0("Focus on interactions of ", sum(colSums(t, 
                na.rm = T) > i, na.rm = T), " most relevant terms (out of ", 
                dim(t)[2], " Begriffen); i=", i))
            t = t[, colSums(t, na.rm = T) > i]
        }
        m = matrix(nrow = dim(t)[1], ncol = dim(t)[2] * dim(t)[2])
        for (i in 1:dim(t)[2]) for (j in 1:dim(t)[2]) m[, ((i - 
            1) * dim(t)[2]) + j] = t[, i] * t[, j]
        colnames(m) = c(paste0(rep(colnames(t), dim(t)[2]), "*", 
            rep(colnames(t), each = dim(t)[2])))
        rownames(m) = rownames(t)
        t = m
        colnames(t) = colnames(m)
        b = cor(data.frame(predictor, t), use = "pairwise.complete")[-1, 
            1]
        names(b) = colnames(m)
    }
    a = sort(a, decreasing = T)
    b = sort(b, decreasing = T)
    c = sort(c(a, b), decreasing = T)
    if (!is.null(min)) 
        c = c[abs(c) >= min]
    if (is.null(min)) 
        c = c[c(1:10, (length(c) - 9):length(c))]
    if (plot & length(c) > 2) 
        spiderplot((c + 1)/2, main = "Correlations with predictor")
    return(a, b, c)
}
