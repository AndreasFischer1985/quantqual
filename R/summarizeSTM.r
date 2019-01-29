#' Function summarizeSTM
#' 
#' Topic summary of stm.
#' @param stm Object of class stm.
#' @param data character vector of documents.
#' @keywords modeling
#' @export
#' @examples
#' 

summarizeSTM <- function (stm, data, topicNo = NULL, main = "Results", stopwords = NULL, 
    cex = 0.5) 
{
    rem2 = ""
    if (!is.null(stopwords)) 
        rem2 = stopwords
    if (is.list(data)) 
        data = as.character(data[[1]])
    data = as.character(data)
    stm.probs.terms <- exp(stm$beta$logbeta[[1]])
    colnames(stm.probs.terms) = stm$vocab
    stm.probs.terms = stm.probs.terms[, is.na(match(colnames(stm.probs.terms), 
        rem2))]
    rownames(stm.probs.terms) = 1:dim(stm.probs.terms)[1]
    stm.probs.topics <- stm$theta
    topicProbability = (colMeans(stm.probs.topics))
    names(topicProbability) = 1:length(topicProbability)
    k = length(topicProbability)
    topics = names(sort(topicProbability, decreasing = T))[1:min(c(k, 
        5))]
    if (is.null(topicNo)) 
        par(mfrow = c(3, 2))
    color = rgb(0/255, 84/255, 122/255)
    k = dim(stm.probs.topics)[2]
    files = 1:dim(stm.probs.topics)[1]
    labels = character(0)
    labels2 = character(0)
    labelmatrix = NULL
    for (i in 1:dim(stm.probs.topics)[2]) {
        s1 = sort((stm.probs.terms)[i, ], decreasing = T)
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
    if (nchar(main) > 200) 
        title = paste(substr(main, 1, 200), "(...)")
    bed = F
    if (!is.null(topicNo)) 
        if (topicNo == 0) 
            bed = T
    if (is.null(topicNo) | bed == T) {
        bp = barplot(s1, horiz = T, main = main, xlim = c(0, 
            1), col = color, cex.names = cex, cex.main = cex + 
            0.1)
        show = dim(stm$theta)[2]
        text(s1, bp, paste0("  ", labels2), srt = 0, cex = cex, 
            pos = 4, col = "grey", xpd = T)
        text(s1, bp, paste0("  ", labels), srt = 0, cex = cex, 
            pos = 4, xpd = T)
        title(sub = paste0(dim(stm$theta)[2], " topics; ", dim(stm$theta)[1], 
            " documents"), cex.sub = cex + 0.2)
    }
    textbreaker = function(text = "Lass uns einen langen Text in eine Textbox setzen, die es in sich hat und viele Zeilen enthaelt.", 
        maxlength = 30, lspace = 1, size = 1, centered = F, separator = "\n     ") {
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
        fac = (th * lspace * 1/dev.size()[2] * 6 * size * length(text))
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
    document.topic = as.matrix(stm$theta)
    colnames(document.topic) = 1:dim(document.topic)[2]
    rownames(document.topic) = paste0("d", 1:dim(document.topic)[1])
    topic.term = as.matrix(exp(stm$beta$logbeta[[1]]))
    colnames(topic.term) = stm$vocab
    stm.probs <- list(terms = topic.term, topics = document.topic)
    topicDescriptions = (apply(labelmatrix, 1, function(x) paste(x, 
        collapse = ",")))
    topicTopDocuments = as.character(data[as.numeric(gsub("d", 
        "", rownames(document.topic)[apply(stm.probs[[2]], 2, 
            function(x) which.max(x))]))])
    doc = as.numeric(gsub("d", "", rownames(document.topic)[apply(stm.probs[[2]], 
        2, function(x) which.max(x))]))
    topicProbability = (colMeans(stm.probs$topics))
    names(topicProbability) = 1:length(topicProbability)
    topics = names(sort(topicProbability, decreasing = T))[1:min(c(length(topicProbability), 
        10))]
    if (is.null(topicNo)) {
        topicNo = as.numeric(topics)
        topicNo = topicNo[topicNo <= 5]
    }
    if (!bed) 
        for (i in topicNo) {
            if (!is.na(topicTopDocuments[i])) {
                plot(1, 1, type = "n", xlab = "", ylab = "", 
                  axes = F)
                text(1, 1, paste0("Document ", doc[i], " (", 
                  round(stm.probs[[2]][paste0("d", doc[i]), i] * 
                    100, 0), "%) :\n\"", textbreaker(text = topicTopDocuments[i], 
                    lspace = 0.2, maxlength = 30, size = 1, centered = F), 
                  "\""), cex = cex + 0.2)
                title(paste0("Topic", i, " (", round(colMeans(stm.probs[[2]])[i] * 
                  100, 0), "%) :\n"), cex.main = cex + 0.5)
                title(paste0("\n\n\n", topicDescriptions[i]), 
                  cex.main = cex)
            }
        }
    return(topicDescriptions)
}
