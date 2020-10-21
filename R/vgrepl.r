#' Function vgrepl
#' 
#' Variant of grepl that takes a vector of patterns.


vgrepl <- function (pattern, x, sumFun = NULL, ...) 
{
    f = Vectorize(grepl, vectorize.arg = "pattern")
    erg = f(pattern = pattern, x = x, ...)
    if (!is.null(sumFun)) 
        erg = sumFun(erg)
    erg
}
