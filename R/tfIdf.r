#' Function tfIdf
#' 
#' Term-Frequency-Inverse-Term-Frequency.
#' @param dtm Document Term Matrix.
#' @param doc Numeric element specifying a document in the dtm.
#' @keywords text mining
#' @export

tfIdf <- function (dtm, doc) 
{
    idf <- log2(nrow(dtm)/col_sums(dtm > 0))
    tf <- as.vector(dtm[doc, ])
    tfIdf <- tf * idf
    names(tfIdf) <- colnames(dtm)
    return(sort(tfIdf, decreasing = T))
}
