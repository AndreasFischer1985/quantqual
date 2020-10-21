#' Function summarizeWordFrequencies
#' 
#' Summarizes word frequencies based on a character vector.
#' @param corpus Character vector containing one document per element.
#' @details Summarizes word frequencies based on a character vector.
#' @keywords text mining
#' @export
#' @examples
#' summarizeWordFrequencies("<1>hello</1> world")

summarizeWordFrequencies <- function (corpus, plot = T) 
{
    frequencies = t(vecToTDM(corpus, min = 0, plot = F))
    min = apply(frequencies, 2, min)
    max = apply(frequencies, 2, max)
    median = apply(frequencies, 2, median)
    mean = apply(frequencies, 2, mean)
    sd = apply(frequencies, 2, sd)
    freq = apply(frequencies, 2, sum)
    docs = apply(frequencies > 0, 2, sum)
    d = data.frame(min, max, median, mean, sd, freq, docs)
    if (plot) 
        quantqual::bp(frequencies, main = "Frequencies", beside = F)
    return(d)
}
