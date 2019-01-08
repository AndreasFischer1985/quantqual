#' Function textbreaker
#' 
#' Adds linebreaks to each element of a character vector.
#' @details Adds linebreaks to each element of a character vector.
#' @keywords helper
#' @export
#' @examples
#' textbreaker("hello world hello world hello world hello world")

labelbreaker <- function (x, length = 30) 
{
    e = character(0)
    for (i in x) e = c(e, textbreaker(i, maxlength = length))
    return(e)
}
