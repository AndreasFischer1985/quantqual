#' Function vgrepl
#' 
#' Variant of grepl that takes a vector of patterns.


vgrepl <- function (...) 
{
    f = Vectorize(grepl, vectorize.arg = "pattern")
    f(...)
}
