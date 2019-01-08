#' Function coherenceLDA
#' 
#' Calculates semantic coherence of an LDA.
#' @param lda Object of class LDA.
#' @param DTM document -term matrix.
#' @keywords modeling
#' @export
#' @examples
#' 

coherenceLDA <- function (lda, DTM, N = 0) 
{
    require(topicmodels)
    if (N == 0) 
        N = dim(posterior(lda)$terms)[2]
    theta = posterior(lda)$terms
    require(Matrix)
    require(slam)
    require(lda)
    if (is.simple_triplet_matrix(DTM)) {
        DTM <- sparseMatrix(i = DTM$i, j = DTM$j, x = DTM$v, 
            dims = c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
    }
    K <- nrow(theta)
    DTMBIN <- DTM > 0
    documentFrequency <- colSums(DTMBIN)
    names(documentFrequency) <- colnames(DTMBIN)
    topNtermsPerTopic <- top.topic.words(theta, N, by.score = TRUE)
    allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))
    DTMBIN <- DTMBIN[, allTopicModelTerms]
    DTMBINCooc <- t(DTMBIN) %*% DTMBIN
    DTMBINCooc <- t((DTMBINCooc + 1)/colSums(DTMBIN))
    DTMBINCooc <- log(DTMBINCooc)
    DTMBINCooc <- as.matrix(DTMBINCooc)
    coherence <- rep(0, K)
    pb <- txtProgressBar(max = K)
    for (topicIdx in 1:K) {
        setTxtProgressBar(pb, topicIdx)
        topWordsOfTopic <- topNtermsPerTopic[, topicIdx]
        coherence[topicIdx] <- 0
        for (m in 2:length(topWordsOfTopic)) {
            for (l in 1:(m - 1)) {
                mTerm <- topWordsOfTopic[m]
                lTerm <- topWordsOfTopic[l]
                coherence[topicIdx] <- coherence[topicIdx] + 
                  DTMBINCooc[mTerm, lTerm]
            }
        }
    }
    close(pb)
    return(coherence)
}
