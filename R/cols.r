#' Function cols
#' 
#' Returns a vector of colors of a certain length.
#' @param num Number of colors to return or a numeric vector containing indices of colors. Defaults to 1.
#' @param col2 Character vector specifying the darker color of the specturm to be returned.
#' @param col1 Character vector specifying the lighter color of the specturm to be returned.
#' @details Returns a vector of colors of a certain length. If col1 and col2 are NULL (default) a rainbow palette is returned. If col2 is specified, col1 is set to "white". If col1 is specified, col2 is set to "black". In addition to regular color labels, the cols-function accepts some pre-defined color-labels (such as "jddm" or "jddmLight" for the Logo-colors of the Journal of Dynamic Decision Making). If num is a numeric vector, the color palette will have max(num) colors, the number of colors returned equals length(num), and num itself is applied as a vector of indices for the color palette returned. 
#' @keywords plotting
#' @export
#' @examples
#' bp(1:10,col=cols(10,"jddm"))

cols <- function (num = 1, col2 = NULL, col1 = NULL) 
{
    if (is.null(num)) 
        num = 1
    if (length(num) > 1) {
        num = num[1]
        warning("Please provide a single number for num (or set col1 and col2 to NULL). Only first element is used.")
    }
    if (length(col1) > 1) {
        col1 = col1[1]
        warning("Please provide a single color name for col1. Only first element is used.")
    }
    if (length(col2) > 1) {
        col2 = col2[1]
        warning("Please provide a single color name for col2. Only first element is used.")
    }
    if (!is.numeric(num) & is.null(col1) & is.null(col2)) {
        col1 = num
        col2 = num
        num = 1
    }
    if (is.numeric(num) & is.null(col1) & is.null(col2)) 
        if (length(num) > 1) {
            num[num < 1] = 1
            return(rainbow(max(num))[num])
        }
        else return(rainbow(num))
    if (is.null(col1)) 
        col1 = "white"
    if (is.null(col2)) 
        col2 = "black"
    x = "jddm"
    y = rgb(135/255, 44/255, 40/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    x = "jddmLight"
    y = rgb(118/255, 119/255, 118/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    x = "fbb"
    y = rgb(0/255, 84/255, 122/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    x = "fbbLight"
    y = rgb(217/255, 231/255, 239/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    x = "bst"
    y = rgb(0/255, 50/255, 100/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    x = "ba"
    y = rgb(226/255, 0/255, 26/255)
    if (col1 == x) 
        col1 = y
    if (col2 == x) 
        col2 = y
    if (length(num) > 1) {
        num[num < 1] = 1
        return(colorRampPalette(c(col1, col2))(max(num))[num])
    }
    if (num == 1) 
        return(col2)
    return(colorRampPalette(c(col1, col2))(num))
}
