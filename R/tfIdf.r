#' Function tfIdf
#' 
#' Term-Frequency-Inverse-Term-Frequency.
#' @param dtm Document Term Matrix.
#' @param docs Numeric element specifying a document in the dtm. If NA (default tfIdf is calculated for each document)
#' @keywords text mining
#' @export

tfIdf <- function (dtm, docs = NA, relative = T, bool = F) 
{
    if (is.null(docs)) 
        docs = 1:nrow(dtm)
    else if (is.na(docs)) 
        docs = 1:nrow(dtm)
    res = sapply(docs, function(x) {
        doc = x
        tf <- as.vector(dtm[doc, ])
        if (bool) 
            tf = as.numeric(tf > 0)
        else if (relative) 
            tf = tf/sum(as.vector(dtm[doc, ]))
        idf <- log2(nrow(dtm)/colSums(dtm > 0))
        tfIdf <- tf * idf
        names(tfIdf) <- colnames(dtm)
        return(sort(tfIdf, decreasing = T))
    })
    return(res)
}
