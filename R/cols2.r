#' Function cols2
#' 
#' Returns a vector of transparent colors based on a vector of colors.
#' @param col Character vector specifying the colors to be transformed.
#' @param transparency Numberic value specifying the transparency of the colors to be returned. Defaults to .3
#' @details Returns a vector of transparent colors based on a vector of colors.
#' @keywords plotting
#' @export
#' @examples
#' cols2(cols(10,"jddm")))

cols2 <- function (col, transparency = 0.3) 
{
    apply(col2rgb(col), 2, function(x) rgb(x[1]/255, x[2]/255, 
        x[3]/255, transparency))
}
