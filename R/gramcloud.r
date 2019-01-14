#' Function gramcloud
#' 
#' Plots wordcloud based on words and/or ngrams.
#' @param x Character vector containing the text of documents.
#' @param ngram Numeric value that determines the maximum order of ngrams that is plotted. Defaults to 1.
#' @param max.num Numeric value that determines the maximum number of ngrams plotted. Defaults to 100.
#' @param min.freq Numeric value that determines the minimum number of occurences for a word or ngram to be plotted. Defaults to 1.
#' @param equal.shares Logical value indicating whether the size of a word or ngram should be relative to all tokens of the same ngram-order (T) or corresponding to the least frequent constituting word or ngram (F). Defaults to T.
#' @param delete Logical value indicating whether ngrams of lower order should be deleted if they are part of an ngram of higher order that is plotted. Defaults to T.
#' @param lowerCase Logical value indicating whether x should be transformed to lower-case. Defaults to T.
#' @param color Color of the most frequent ngram. Defaults to "red".
#' @param ... Additional graphical parameters for wordcloud.
#' @details Plots wordcloud based on words and/or ngrams. By default words that are part of plotted ngrams are not plotted (delete=T).
#' @keywords plotting
#' @export
#' @examples
#' gramcloud(c("This is an exemplary text","This is too","And this is probably the longest text"),ngram=2,min.freq=1)

gramcloud <- function (x = NULL, ngram = 1, max.num = 100, min.freq = 1, equal.shares = T, 
    delete = T, lowerCase = T, main = "Cloud of n-grams", color = "red", 
    random.order = FALSE, rot.per = 0.2, scale = c(4, 0.5), use.r.layout = T, 
    ordered.colors = T, ...) 
{
    if (lowerCase) {
        data = tolower(x)
    }
    else data = x
    if (length(data) == 0) 
        data = c("This is an exemplary text", "This is too", 
            "And this is probably the longest text")
    require(stringr)
    l = list()
    for (i in 1:ngram) {
        BigramTokenizer <- function(x) unlist(quanteda::tokens_ngrams(quanteda::tokens(x), 
            i))
        l[[i]] = sort(table(BigramTokenizer(gsub("\\W", " ", 
            data))), decreasing = T)
    }
    if (!equal.shares) 
        if (length(l) > 1) 
            for (j in 1:(length(l) - 1)) for (i in 1:length(l[[j + 
                1]])) if (length(!is.na(c(str_match(names(l[[j + 
                1]])[i], names(l[[j]]))))) > 0) {
                l[[j + 1]][i] = min(l[[j]][which(!is.na(c(str_match(names(l[[j + 
                  1]])[i], names(l[[j]])))))])
            }
    for (i in 1:length(l)) {
        l[[i]][l[[i]] < min.freq] = NA
        l[[i]] = (l[[i]][!is.na(l[[i]])])
    }
    l = l[sapply(l, length) > 0]
    if (equal.shares) 
        l = lapply(l, function(x) x/max(x))
    if (delete) 
        if (length(l) > 1) 
            for (j in 1:(length(l) - 1)) for (i in 1:length(l[[j]])) if (max(grepl(names(l[[j]])[i], 
                names(l[[j + 1]])), na.rm = T)) {
                l[[j]][i] = NA
            }
    freq = sort(unlist(l), decreasing = T)
    if (length(freq) > max.num) 
        freq = freq[1:max.num]
    set.seed(0)
    wordcloud::wordcloud(names(freq), freq/max(freq), random.order = random.order, 
        rot.per = rot.per, min.freq = 0, scale = scale, use.r.layout = use.r.layout, 
        ordered.colors = ordered.colors, colors = colorRampPalette(c(color, 
            "lightgrey"))(length(freq)), ...)
    title(main)
    return(freq)
}
