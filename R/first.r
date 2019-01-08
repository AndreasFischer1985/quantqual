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
x[[1 + s]]
