#' Function summarizeCodeFrequencies
#' 
#' Summarizes code frequencies based on a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Summarizes code frequencies based on a character vector.
#' @keywords text mining
#' @export
#' @examples
#' summarizeCodeFrequencies("<1>hello</1> world")

summarizeCodeFrequencies <- function (corpus, plot = T) 
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
    frequencies = (table(data.frame(subcorpus = c1, code = m1)))
    d = data.frame()
    if (sum(dim(frequencies)) > 0) {
        min = apply(frequencies, 2, min)
        max = apply(frequencies, 2, max)
        median = apply(frequencies, 2, median)
        mean = apply(frequencies, 2, mean)
        sd = apply(frequencies, 2, sd)
        freq = apply(frequencies, 2, sum)
        docs = apply(frequencies > 0, 2, sum)
        d = data.frame(min, max, median, mean, sd, freq, docs)
        if (plot) 
            quantqual::bp(frequencies, main = "Frequencies", 
                beside = F)
    }
    return(d)
}
