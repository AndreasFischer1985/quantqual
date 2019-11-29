#' Function last
#' 
#' Returns the last element of a vector.
#' @param x A vector or list.
#' @details Returns the last element of a vector.
#' @keywords preprocessing
#' @export
#' @examples
#' last(c(1,2,3))

last <- function (x, s = 0) 
if (is.data.frame(x) | is.matrix(x)) return(ifelse(dim(x)[1] > 
    0, list(x[, dim(x)[2] - s]), list(NA))[[1]]) else return(ifelse(length(x) > 
    0, x[[length(x) - s]], NA))
