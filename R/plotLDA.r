#' Function plotLDA
#' 
#' Plots topic document distribution and topic descriptions.
#' @param lda Object of class LDA.
#' @param labels character vector with document labels.
#' @param srt Numeric value specifying the rotation of the x-axis (between 0 and 360 degrees). Defaults to 45.
#' @details Plots topic document distribution and topic descriptions based on a topicmodels::LDA object.
#' @keywords plotting
#' @export
#' @examples
#' l=af.lda(c("Hello world","Hallo Welt"));plotLDA(l)

plotLDA <- function (lda = NULL, labels = NULL, main = "Topic distribution over documents", 
    border = NA, space = 0, stopwords = NULL, cex = 0.7, srt = 45) 
{
    require(topicmodels)
    if (is.null(labels)) 
        labels = rownames(posterior(lda)$topics)
    lda.probs.terms = posterior(lda)$terms
    rem2 = NULL
    if (!is.null(stopwords)) 
        rem2 = stopwords
    lda.probs.terms = lda.probs.terms[, is.na(match(colnames(lda.probs.terms), 
        rem2))]
    lda.top.terms = apply(lda.probs.terms, 1, function(x) names(sort(x, 
        decreasing = T)[1:5]))
    topic.descriptions = apply(lda.top.terms, 2, function(x) paste(x, 
        collapse = ", "))
    bp = barplot(t(posterior(lda)$topics), names.arg = rep("", 
        dim(posterior(lda)$topics)[1]), col = rainbow(dim(posterior(lda)$topics)[2]), 
        xlim = c(0, dim(posterior(lda)$topics)[1] * 1.5), main = main, 
        border = border, space = space)
    legend("right", paste0("Topic ", 1:dim(posterior(lda)$topics)[2], 
        "\n(", gsub(", ", ",\n", topic.descriptions), ")\n"), 
        bty = "n", fill = rainbow(dim(posterior(lda)$topics)[2]), 
        cex = cex)
    if (length(bp) <= 30) 
        text(bp + 0.5, 0, paste(labels, " "), srt = srt, pos = 2, 
            xpd = T, cex = 0.8)
}
