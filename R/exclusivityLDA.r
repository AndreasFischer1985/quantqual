#' Function exclusivityLDA
#' 
#' Calculates exclusivity of an LDA.
#' @param lda Object of class LDA.
#' @param DTM document -term matrix.
#' @keywords modeling
#' @export
#' @examples
#' 

exclusivityLDA <- function (lda, dtm, lambda = 0, num.words = 0) 
{
    require(topicmodels)
    if (num.words == 0) 
        num.words = dim(posterior(lda)$terms)[2]
    pwt = posterior(lda)$terms
    pw <- colSums(dtm)/sum(dtm)
    apply(pwt, 1, function(x, num.words, pw, lambda) {
        x <- lambda * log(x) + (1 - lambda) * log(x/pw)
        return((sort(x, decreasing = T)[1:num.words]))
    }, num.words, pw, lambda)
}
