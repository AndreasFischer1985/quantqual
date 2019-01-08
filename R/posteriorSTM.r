#' Function posteriorSTM
#' 
#' Posterior for STM topic models.
#' @param tm topic model object of class stm or LDA.
#' @keywords modeling
#' @export
#' @examples
#' 

posteriorSTM <- function (tm) 
{
    if (class(tm) == "STM") {
        stm.probs.terms <- exp(tm$beta$logbeta[[1]])
        colnames(stm.probs.terms) = tm$vocab
        rownames(stm.probs.terms) = 1:dim(stm.probs.terms)[1]
        stm.probs.topics <- tm$theta
        topicProbability = (colMeans(stm.probs.topics))
        names(topicProbability) = 1:length(topicProbability)
        topics = names(sort(topicProbability, decreasing = T))[1:min(c(length(topicProbability), 
            5))]
        document.topic = as.matrix(tm$theta)
        colnames(document.topic) = 1:dim(document.topic)[2]
        rownames(document.topic) = paste0("d", 1:dim(document.topic)[1])
        topic.term = as.matrix(exp(tm$beta$logbeta[[1]]))
        colnames(topic.term) = tm$vocab
        stm.probs <- list(terms = topic.term, topics = document.topic)
        return(stm.probs)
    }
    return(NULL)
}
