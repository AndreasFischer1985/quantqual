#' Function first
#' 
#' Returns the first element of a vector.
#' @param x A vector or list.
#' @details Returns the first element of a vector.
#' @keywords preprocessing
#' @export
#' @examples
#' first(c(1,2,3))

first <- function (x, s = 0, factorsAsStrings = T) 
{
    if (factorsAsStrings == T) 
        if (is.factor(x)) 
            x = as.character(x)
    if (is.data.frame(x) | is.matrix(x)) 
        r = ifelse(dim(x)[1] > 0, list(x[, 1 + s]), list(NA))[[1]]
    else r = ifelse(length(x) > 0, x[[1 + s]], NA)
    if (factorsAsStrings == T) 
        if (is.factor(r)) 
            r = as.character(r)
    return(r)
}
