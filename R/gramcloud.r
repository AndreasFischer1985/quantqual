#' Function gramcloud
#' 
#' Plots wordcloud based on words and/or ngrams.
#' @param x Character vector containing the text of documents.
#' @keywords ngram Numeric value that determines the maximum order of ngrams that is plotted. Defaults to 2.
#' @keywords min.freq Numeric value that determines the minimum number of occurences for a word or ngram to be plotted. Defaults to 1.
#' @keywords equal.shares Logical value indicating whether the size of a word or ngram should be relative to all tokens of the same ngram-order (T) or corresponding to the least frequent constituting word or ngram (F). Defaults to T.
#' @keywords delete Logical value indicating whether ngrams of lower order should be deleted if they are part of an ngram of higher order that is plotted. Defaults to T.
#' @details Plots wordcloud based on words and/or ngrams. By default words that are part of plotted ngrams are not plotted (delete=T).
#' @keywords plotting
#' @export
#' @examples
#' gramcloud(c("This is an exemplary text","This is too","And this is probably the longest text"),ngram=2,min.freq=1)

gramcloud <- function (x = NULL, ngram = 3, min.freq = 1, equal.shares = T, 
    delete = T) 
{
    data = x
    if (is.null(data)) 
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
    if (length(freq) > 100) 
        freq = freq[1:100]
    set.seed(0)
    wordcloud::wordcloud(names(freq), freq/max(freq), random.order = FALSE, 
        rot.per = 0.2, min.freq = 0, scale = c(1.1, 0.1), use.r.layout = T, 
        ordered.colors = T, colors = colorRampPalette(c("red", 
            "lightgrey"))(length(freq)))
    title("Cloud of n-grams")
    return(freq)
}
