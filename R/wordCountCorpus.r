#' Function wordCountCorpus
#' 
#' Returns word frequencies based on a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Returns word frequencies based on a character vector.
#' @keywords text mining
#' @export
#' @examples
#' wordCountCorpus("<1>hello</1> world")

wordCountCorpus <- function (corpus, lowerCase = T, min = 0, sort = T, plot = T) 
{
    uncodeCorpus = function(corpus = c("Die Veranstaltung war toll", 
        "Sehr gelungen, wirklich toll.", "Toll. Einfach <c1>toll</c1>"), 
        mess = T, assign = T) {
        for (i in 1:length(corpus)) {
            corpus[i] = gsub("<[/]?[A-Za-z0-9]+>", "", corpus[i])
        }
        if (mess) 
            message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
        if (assign) 
            assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
        return(corpus)
    }
    corpus = uncodeCorpus(corpus, mess = F, assign = F)
    if (lowerCase) 
        corpus = tolower(corpus)
    dtm = paste(corpus, collapse = " ")
    dtm = gsub("\n", " ", dtm)
    dtm = gsub("\\W", " ", dtm)
    dtm = gsub("-", " ", dtm)
    for (r in 1:2) dtm = gsub("  ", " ", dtm)
    s = strsplit(dtm, " ")
    s = s[[1]][s[[1]] != ""]
    if (!sort) 
        s = table(s)
    if (sort) 
        s = sort(table(s), decreasing = T)
    if (!is.null(min)) 
        s1 = s[s >= min]
    if (is.null(min)) 
        s1 = s[c(1:10, (length(s) - 9):length(s))]
    if (is.null(dim(s1))) {
        dim(s1) = c(1, length(s1))
        rownames(s1) = names(s)[s >= min]
    }
    if (plot) 
        bp = bp(c(s1), main = "Word frequencies")
    return(s)
}
