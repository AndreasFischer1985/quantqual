#' Function plotSTM
#' 
#' Plots topic document distribution and topic descriptions based on an stm object.
#' @param stm Object of class stm.
#' @param labels character vector with document labels.
#' @param srt Numeric value specifying the rotation of the x-axis (between 0 and 360 degrees). Defaults to 45.
#' @details Plots topic document distribution and topic descriptions based on an stm object.
#' @keywords plotting
#' @export
#' @examples
#' s=af.stm(c("Hello world","Hallo Welt"));plotSTM(s)

plotSTM <- function (stm = NULL, labels = NULL, main = "Topic distribution over documents", 
    border = NA, space = 0, stopwords = NULL, cex = 0.7, srt = 45, 
    plot = T) 
{
    if (is.null(labels)) 
        labels = rownames(posteriorSTM(stm)$topics)
    stm.probs.terms = exp(stm$beta$logbeta[[1]])
    colnames(stm.probs.terms) = stm$vocab
    rem2 = NULL
    if (!is.null(stopwords)) 
        rem2 = c(stopwords)
    stm.probs.terms = stm.probs.terms[, is.na(match(colnames(stm.probs.terms), 
        rem2))]
    stm.top.terms = apply(stm.probs.terms, 1, function(x) names(sort(x, 
        decreasing = T)[1:5]))
    topic.descriptions = apply(stm.top.terms, 2, function(x) paste(x, 
        collapse = ", "))
    if (plot) {
        bp = barplot(t(posteriorSTM(stm)$topics), names.arg = rep("", 
            dim(posteriorSTM(stm)$topics)[1]), col = rainbow(dim(posteriorSTM(stm)$topics)[2]), 
            xlim = c(0, dim(posteriorSTM(stm)$topics)[1] * 1.5), 
            main = main, border = border, space = space)
        if (dim(posteriorSTM(stm)$topics)[2] < 5) 
            legend("right", paste0("Topic ", 1:dim(posteriorSTM(stm)$topics)[2], 
                "\n(", gsub(", ", ",\n", topic.descriptions), 
                ")\n"), bty = "n", fill = rainbow(dim(posteriorSTM(stm)$topics)[2]), 
                cex = cex)
        else legend("right", paste0("Topic ", 1:dim(posteriorSTM(stm)$topics)[2]), 
            bty = "n", fill = rainbow(dim(posteriorSTM(stm)$topics)[2]), 
            cex = cex)
        if (length(bp) <= 30) 
            text(bp + 0.5, 0, paste(labels, " "), srt = srt, 
                pos = 2, xpd = T, cex = 0.8)
    }
    invisible(topic.descriptions)
}
