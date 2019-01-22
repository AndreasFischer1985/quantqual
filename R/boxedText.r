#' Function boxedText
#' 
#' Adds a boxed text to a plot.
#' @param x Numeric value specifying the text's position on the x-axis.
#' @param y Numeric value specifying the text's position on the y-axis.
#' @param x2 Numeric value specifying the arrow's position on the x-axis.
#' @param y2 Numeric value specifying the arrow's position on the y-axis.
#' @param txt Character value specifying the text to be plotted.
#' @param col Character value specifying the color of the box. Defaults to "yellow".
#' @param pos Numeric value specifying the position of the arrow relative to the box (0=no arrow plotted, 1=down, 2=left, 3=up, 4=right).
#' @param ... Additional graphical parameters for text.
#' @details Adds a boxed text to a plot.
#' @keywords plotting
#' @export
#' @examples
#' plot(1:10,1:10);boxedText(7,2.5,3,3,,txt="This (3;3) is a remarkable point worth emphasizing",col="yellow",maxlength=20)

boxedText <- function (x, y, x2 = NULL, y2 = NULL, txt = "Hallo, du\nWelt", 
    maxlength = NULL, col = "yellow", pos = NULL, font = 1, hspace = 1, 
    vspace = 1.2, cex = 1, ...) 
{
    if (is.null(x2) | is.null(y2)) 
        pos = NA
    if (!is.null(pos)) 
        if (!is.numeric(pos)) 
            pos = NA
        else if (pos > 4) 
            pos = NA
    if (!is.null(maxlength)) 
        txt = quantqual::trim(quantqual::textbreaker(txt, maxlength = maxlength))
    sw = strwidth(txt, font = font, cex = cex, units = "user") * 
        hspace
    sh = strheight(txt, font = font, cex = cex, units = "user") * 
        vspace
    if (is.null(pos)) 
        if ((y2 <= y + sh/2 & y2 >= y - sh/2 & x2 <= x + sw/2 & 
            x2 >= x - sw/2)) 
            pos = NA
        else if (abs(y2 - y) > abs(x2 - x)) {
            if ((y2 - y) < 0) 
                pos = 1
            else pos = 3
        }
        else if (abs(x2 - x) >= abs(y2 - y)) {
            if ((x2 - x) < 0) 
                pos = 2
            else pos = 4
        }
    if (is.na(pos)) {
        polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2), 
            y = c(y + sh/2, y + sh/2, y - sh/2, y - sh/2), col = col)
    }
    else {
        if (pos == 0) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2), 
                y = c(y + sh/2, y + sh/2, y - sh/2, y - sh/2), 
                col = col)
        if (pos == 1) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x + sw/6, 
                x2, x - sw/6, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y - sh/2, y - sh/2, y2, y - sh/2, y - sh/2), 
                col = col)
        if (pos == 2) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2, 
                x - sw/2, x2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y - sh/2, y - sh/2, y - sh/6, y2, y + sh/6), 
                col = col)
        if (pos == 3) 
            polygon(x = c(x - sw/2, x - sw/6, x2, x + sw/6, x + 
                sw/2, x + sw/2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y2, y + sh/2, y + sh/2, y - sh/2, y - sh/2), 
                col = col)
        if (pos == 4) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x2, x + 
                sw/2, x + sw/2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y + sh/6, y2, y - sh/6, y - sh/2, y - sh/2), 
                col = col)
    }
    text(x, y, txt, cex = cex, font = font, ...)
}
