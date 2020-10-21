#' Function mar
#' 
#' Returns margins of plots given numeric vectors to add and/or multiply, and/or a set of labels for each axis.
#' @param mar Numeric vector containing default margins. Defaults to c(5.1, 4.1, 4.1, 2.1).
#' @param mar1 Numeric vector to be multiplied with mar. Defaults to c(1,1,1,1).
#' @param mar0 Numeric vector to be added to the product of mar and mar1. Defaults to c(0,0,0,0).
#' @param labels1 Character vector containing labels for axis 1 (bottom).
#' @param labels2 Character vector containing labels for axis 2 (left).
#' @param labels3 Character vector containing labels for axis 3 (up).
#' @param labels4 Character vector containing labels for axis 4 (right).
#' @param set Logical value specifying whether to set the margins after determining them. Defaults to T
#' @details Returns margins of plots given numeric vectors to add and/or multiply, and/or a set of labels for each axis.
#' @keywords plotting
#' @export
#' @examples
#' mar();

mar <- function (mar = c(5.1, 4.1, 4.1, 2.1), mar1 = c(1, 1, 1, 1), 
    mar0 = c(0, 0, 0, 0), labels1 = NULL, labels2 = NULL, labels3 = NULL, 
    labels4 = NULL, labelFactor = 3, set = T) 
{
    if (is.null(mar)) 
        if (length(dev.list()) > 0) {
            mar = par("mar")
        }
        else mar = c(5.1, 4.1, 4.1, 2.1)
    if (!is.null(labels1)) 
        mar[1] = mar[1] + max(strwidth(labels1, units = "inches") * 
            labelFactor, na.rm = T)
    if (!is.null(labels2)) 
        mar[2] = mar[2] + max(strwidth(labels2, units = "inches") * 
            labelFactor, na.rm = T)
    if (!is.null(labels3)) 
        mar[3] = mar[3] + max(strwidth(labels3, units = "inches") * 
            labelFactor, na.rm = T)
    if (!is.null(labels4)) 
        mar[4] = mar[4] + max(strwidth(labels4, units = "inches") * 
            labelFactor, na.rm = T)
    mar = mar * mar1
    mar = mar + mar0
    if (set != F) 
        par(mar = mar)
    return(invisible(mar))
}
