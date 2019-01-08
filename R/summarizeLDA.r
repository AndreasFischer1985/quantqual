#' Function summarizeLDA
#' 
#' Topic summary of LDA.
#' @param lda Object of class LDA.
#' @param data character vector of documents.
#' @param dtm document-term matrix
#' @keywords modeling
#' @export
#' @examples
#'  

summarizeLDA <- function (lda, data, dtm, main = "Ergebnisse", stopwords = NULL) 
{
    rem2 = ""
    if (!is.null(stopwords)) 
        rem2 = stopwords
    if (is.list(data)) 
        data = as.character(data[[1]])
    d = dtm
    data = as.character(data)
    ldaPost = posterior(lda)
    ldaPost$terms = ldaPost$terms[, is.na(match(colnames(ldaPost$terms), 
        rem2))]
    color = rgb(0/255, 84/255, 122/255)
    files = 1:dim(ldaPost$topics)[1]
    k = dim(ldaPost$topics)[2]
    par(mfrow = c(ceiling(sqrt(k)), ceiling(sqrt(k + 1))))
    topicProbability = (colMeans(ldaPost$topics))
    labels = character(0)
    labels2 = character(0)
    labelmatrix = NULL
    for (i in 1:dim(ldaPost$topics)[2]) {
        s1 = sort((ldaPost$terms)[i, ], decreasing = T)
        s2 = c(s1[-length(s1)] - s1[-1], 0)
        cut2 = s1[which.max(s2)]
        labels = c(labels, paste(names(s1)[1:5][s1[1:5] >= cut2], 
            collapse = ","))
        labels2 = c(labels2, paste(names(s1)[1:5], collapse = ","))
        if (i == 1) 
            labelmatrix = names(s1)[1:5]
        if (i > 1) 
            labelmatrix = data.frame(labelmatrix, names(s1)[1:5])
    }
    labelmatrix = t(labelmatrix)
    rownames(labelmatrix) = NULL
    s1 = sort(topicProbability, decreasing = F)
    labels = labels[as.numeric(names(s1))]
    labels2 = labels2[as.numeric(names(s1))]
    bp = barplot(s1, horiz = T, main = main, xlim = c(0, 1), 
        col = color, cex.names = 0.5, cex.main = 0.6)
    show = dim(ldaPost$topics)[2]
    text(s1, bp, paste0("  ", labels2), srt = 0, cex = 0.5, pos = 4, 
        col = "grey", xpd = T)
    text(s1, bp, paste0("  ", labels), srt = 0, cex = 0.5, pos = 4, 
        xpd = T)
    title(sub = paste0(k, " topics; ", dim(ldaPost$topics)[1], 
        " documents; alpha = ", round(slot(lda, "alpha"), 2)), 
        cex.sub = 0.7)
    textbreaker = function(text = "Lass uns einen langen Text in eine Textbox setzen, die es in sich hat und viele Zeilen enthaelt.", 
        zeilenabstand = 1, maxlength = 30, size = 1, centered = F, 
        separator = "\n     ") {
        if (nchar(text) > maxlength * 4) 
            text = paste(substr(text, 1, maxlength * 4), "(...)")
        count = 1
        while (nchar(text[length(text)]) > maxlength) {
            count = count + 1
            if (count > 23) 
                break
            spacePos = as.vector(gregexpr("( |\n|/)", text[length(text)])[[1]])
            spacePos = spacePos[spacePos < maxlength]
            spacePos = spacePos[length(spacePos)]
            if (length(spacePos) == 0) {
                spacePos = as.vector(gregexpr(" ", text[length(text)])[[1]])
                spacePos = spacePos[length(spacePos)]
            }
            if (length(spacePos) == 0) 
                break
            text = c(text[-length(text)], substr(text[length(text)], 
                1, spacePos), substr(text[length(text)], spacePos + 
                1, nchar(text[length(text)])))
        }
        p1 = par("mai")
        p2 = p1 * 0
        par(mai = p2)
        th = par()$ps * 1/72
        fac = (th * zeilenabstand * 1/dev.size()[2] * 6 * size * 
            length(text))
        pos = NULL
        if (!centered) 
            pos = 4
        x = 0.05
        if (centered) 
            x = 0.5
        y = (length(text):1)/length(text) * fac + (1 - fac) - 
            (1/dev.size()[2]^2)
        lines = length(text)
        text = paste0(text, collapse = separator)
        y = 0.5
        par(mai = p1)
        return(text)
    }
    lda.terms <- labelmatrix
    lda.probs <- posterior(lda)
    topicDescriptions = (apply(lda.terms, 1, function(x) paste(x, 
        collapse = ",")))
    topicTopDocuments = as.character(data[as.numeric(gsub("d", 
        "", rownames(d)[apply(lda.probs[[2]], 2, function(x) which.max(x))]))])
    doc = as.numeric(gsub("d", "", rownames(d)[apply(lda.probs[[2]], 
        2, function(x) which.max(x))]))
    topicProbability = (colMeans(lda.probs$topics))
    names(topicProbability) = 1:length(topicProbability)
    topics = names(sort(topicProbability, decreasing = T))[1:min(c(length(topicProbability), 
        10))]
    for (i in as.numeric(topics)) {
        if (!is.na(topicTopDocuments[i])) {
            plot(1, 1, type = "n", xlab = "", ylab = "", axes = F)
            text(1, 1, paste0("Document ", doc[i], " (", round(lda.probs[[2]][paste0("d", 
                doc[i]), i] * 100, 0), "%) :\n\"", textbreaker(text = topicTopDocuments[i], 
                zeilenabstand = 0.2, maxlength = 30, size = 1, 
                centered = F), "\""), cex = 0.8)
            title(paste0("Topic", i, " (", round(colMeans(lda.probs[[2]])[i] * 
                100, 0), "%) :"), cex.main = 1)
            title(paste0("\n\n\n", topicDescriptions[i]), cex.main = 0.5)
        }
    }
}
