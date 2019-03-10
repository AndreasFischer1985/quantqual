#' Function descriptionsLDA
#' 
#' Returns topic descriptions.
#' @param lds topicmodels::LDA
#' @param n Numeric value specifying the number of terms to describe each topic.
#' @details Returns topic descriptions.
#' @keywords plotting
#' @export

descriptionsLDA <- function (lda, n = 5) 
{
    topic.descriptions = apply(topicmodels::terms(lda, n), 2, 
        function(x) paste(x, collapse = ", "))
    return(topic.descriptions)
}
