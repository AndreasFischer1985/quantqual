#' Function retrieveCodeFrequencies
#' 
#' Retrieves all codes in a character vector and returns frequencies of codings.
#' @param corpus Character vector containing one document per element.
#' @details Retrieves all codes in a character vector and returns frequencies of codings.
#' @keywords text mining
#' @export
#' @examples
#' retrieveCodeFrequencies("<1>hello</1> world")

retrieveCodeFrequencies <- function (corpus, byString = T, binary = F, plot = T) 
{
    library(stringr)
    coding = paste0("<[A-Za-z0-9]+>")
    m = str_match_all(corpus, coding)
    m1 = character(0)
    c1 = numeric(0)
    for (i in 1:length(corpus)) {
        m1 = c(m1, m[[i]][, 1])
        c1 = c(c1, rep(i, length(m[[i]][, 1])))
    }
    frequencies = NULL
    if (byString) 
        frequencies = (table(data.frame(subcorpus = c1, code = m1)))
    if (!byString) 
        frequencies = data.frame(code = table(as.factor(m1)))
    if (sum(dim(frequencies)) > 0) {
        if (binary) 
            frequencies = apply(frequencies > 0, c(1, 2), as.numeric)
        if (plot) 
            barplot(frequencies, main = "Frequencies")
    }
    return(frequencies)
}
