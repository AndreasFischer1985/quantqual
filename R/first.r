#' Function first
#' 
#' Returns the first element of a vector.
#' @param x A vector or list.
#' @details Returns the first element of a vector.
#' @keywords preprocessing
#' @export
#' @examples
#' first(c(1,2,3))

first <- function (x, s = 0) 
if (is.data.frame(x) | is.matrix(x)) return(ifelse(dim(x)[1] > 
    0, list(x[, 1 + s]), list(NA))[[1]]) else return(ifelse(length(x) > 
    0, x[[1 + s]], NA))
