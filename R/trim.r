#' Function trim
#' 
#' Trims elements of a character vector.
#' @param x A character vector.
#' @details Trims elements of a character vector.
#' @keywords preprocessing
#' @export
#' @examples
#' trim(" Hi  ")

trim <- function (x) 
gsub("(^[ \n\r\t]+|[ \n\r\t]+$)", "", x)
