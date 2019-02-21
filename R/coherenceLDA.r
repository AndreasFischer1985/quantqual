#' Function coherenceLDA
#' 
#' Calculates semantic coherence of an LDA.
#' @param lda Object of class LDA.
#' @param dtm document-term matrix.
#' @param n Number of terms for determining document-coocurence. If 0 (default) n is set to the number of terms in the dtm.
#' @keywords modeling
#' @export
#' 

coherenceLDA <- function (lda, dtm, n = 0) 
{
    theta = topicmodels::posterior(lda)$terms
    if (n == 0) 
        n = dim(topicmodels::posterior(lda)$terms)[2]
    k <- nrow(theta)
    normalized.topics <- theta/(rowSums(theta) + 1e-05)
    scores <- apply(normalized.topics, 2, function(x) x * (log(x + 
        1e-05) - sum(log(x + 1e-05))/length(x)))
    top.terms <- matrix(apply(scores, 1, function(x) colnames(scores)[order(x, 
        decreasing = TRUE)[1:n]]), ncol = k)
    dtm.binary <- dtm > 0
    doc.frequency <- colSums(dtm.binary)
    names(doc.frequency) <- colnames(dtm.binary)
    unique.terms <- unique(as.vector(top.terms))
    dtm.binary <- dtm.binary[, unique.terms]
    dtm.binary.product <- t(dtm.binary) %*% dtm.binary
    dtm.binary.product <- t((dtm.binary.product + 1)/colSums(dtm.binary))
    dtm.binary.product <- log(dtm.binary.product)
    dtm.binary.product <- as.matrix(dtm.binary.product)
    coherence <- rep(0, k)
    for (id in 1:k) {
        top.terms.topic <- top.terms[, id]
        coherence[id] <- 0
        for (m in 2:length(top.terms.topic)) {
            for (l in 1:(m - 1)) {
                coherence[id] <- coherence[id] + dtm.binary.product[top.terms.topic[m], 
                  top.terms.topic[l]]
            }
        }
    }
    return(coherence)
}
