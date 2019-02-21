#' Function exclusivityLDA
#' 
#' Calculates exclusivity of an LDA.
#' @param lda Object of class LDA.
#' @param DTM document -term matrix.
#' @keywords modeling
#' @export
#' @examples
#' 

exclusivityLDA <- function (lda, dtm, lambda = 0, num.terms = 0) 
{
    require(topicmodels)
    if (num.terms == 0) 
        num.terms = dim(posterior(lda)$terms)[2]
    postt = posterior(lda)$terms
    frac <- colSums(dtm)/sum(dtm)
    apply(postt, 1, function(x, num.terms, frac, lambda) {
        x <- lambda * log(x) + (1 - lambda) * log(x/frac)
        return((sort(x, decreasing = T)[1:num.terms]))
    }, num.terms, frac, lambda)
}
