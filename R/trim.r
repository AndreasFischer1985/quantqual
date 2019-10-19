#' Function trim
#' 
#' Trims elements of a character vector.
#' @param x A character vector.
#' @details Trims elements of a character vector.
#' @keywords preprocessing
#' @export
#' @examples
#' trim(getHTML(" Hi  "

trim <- function (x) 
gsub("(^[ \n\r]+|[ \n\r]+$)", "", x)
