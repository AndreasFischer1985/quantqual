#' Function last
#' 
#' Returns the last element of a vector.
#' @param x A vector or list.
#' @details Returns the last element of a vector.
#' @keywords preprocessing
#' @export
#' @examples
#' last(c(1,2,3))

last <- function (x, s = 0, factorsAsStrings = T) 
{
    if (factorsAsStrings == T) 
        if (is.factor(x)) 
            x = as.character(x)
    if (is.data.frame(x) | is.matrix(x)) 
        r = ifelse(dim(x)[1] > 0, list(x[, dim(x)[2] - s]), list(NA))[[1]]
    else r = ifelse(length(x) > 0, x[[length(x) - s]], NA)
    if (factorsAsStrings == T) 
        if (is.factor(r)) 
            r = as.character(r)
    return(r)
}
