#' Function boxedText
#' 
#' Adds a boxed text to a plot.
#' @param x Numeric value specifying the text's position on the x-axis.
#' @param y Numeric value specifying the text's position on the y-axis.
#' @param x2 Numeric value specifying the arrow's position on the x-axis.
#' @param y2 Numeric value specifying the arrow's position on the y-axis.
#' @param txt Character value specifying the text to be plotted.
#' @param col Character value specifying the color of the box. Defaults to "yellow".
#' @param col Character value specifying the color of the border. Defaults to "black".
#' @param pos Numeric value specifying the position of the arrow relative to the box (0=no arrow plotted, 1=down, 2=left, 3=up, 4=right).
#' @param ... Additional graphical parameters for text.
#' @details Adds a boxed text to a plot.
#' @keywords plotting
#' @export
#' @examples
#' plot(1:10,1:10);boxedText(7,2.5,text="This (3;3) is a remarkable point worth emphasizing",3,3,col="yellow",maxlength=20)

boxedText <- function (x, y, text = "Hello \nWorld", x2 = NULL, y2 = NULL, 
    maxlength = NULL, col = "yellow", border = "black", pos = NULL, 
    font = 1, hspace = 1, vspace = 1.2, cex = 1, decollide = F, 
    decollide.repetitions = 10, ...) 
{
    if (length(x) > 1 | length(y) > 1) {
        if (length(y) > length(x)) 
            x = rep(x[1], length(y))
        if (length(y) < length(x)) 
            y = rep(y[1], length(x))
        if (length(text) != length(x)) 
            text = rep(text[1], length(x))
        if (is.null(x2)) 
            x2 = x
        if (is.null(y2)) 
            y2 = y
        if (length(x2) != length(x)) 
            x2 = rep(x2[1], length(x))
        if (length(y2) != length(x)) 
            y2 = rep(y2[1], length(x))
        if (length(col) != length(x)) 
            col = rep(col[1], length(x))
        if (length(border) != length(x)) 
            border = rep(border[1], length(x))
        if (length(pos) != length(x)) 
            pos = rep(pos[1], length(x))
        if (length(cex) != length(x)) 
            cex = rep(cex[1], length(x))
        if (!is.null(maxlength)) 
            for (i in 1:length(x)) text[i] = quantqual::trim(quantqual::textbreaker(text[i], 
                maxlength = maxlength))
        if (decollide) {
            print(paste0("x=", x, "\ny=", y, "\ntext=", text))
            d = quantqual::decollide(x, y, text, repetitions = decollide.repetitions)
            x = d[, "x"]
            y = d[, "y"]
        }
        for (i in 1:length(x)) boxedText(x = x[i], y = y[i], 
            text = text[i], x2 = x2[i], y2 = y2[i], maxlength = maxlength, 
            col = col[i], border = border[i], pos = pos[i], font = font, 
            hspace = hspace, vspace = vspace, cex = cex[i], ...)
        return()
    }
    if (is.null(x2) | is.null(y2)) 
        pos = NA
    if (!is.null(pos)) 
        if (!is.numeric(pos)) 
            pos = NA
        else if (pos > 4) 
            pos = NA
    if (!is.null(maxlength)) 
        text = quantqual::trim(quantqual::textbreaker(text, maxlength = maxlength))
    if (decollide) {
        d = quantqual::decollide(x, y, text, repetitions = decollide.repetitions)
        x = d[, "x"]
        y = d[, "y"]
    }
    sw = strwidth(text, font = font, cex = cex, units = "user") * 
        hspace
    sh = strheight(text, font = font, cex = cex, units = "user") * 
        vspace
    if (is.null(pos)) 
        if ((y2 <= y + sh/2 & y2 >= y - sh/2 & x2 <= x + sw/2 & 
            x2 >= x - sw/2)) 
            pos = NA
        else if (abs(y2 - y) - sh/2 > abs(x2 - x) - sw/2) {
            if ((y2 - y) < 0) 
                pos = 1
            else pos = 3
        }
        else if (abs(x2 - x) - sw/2 >= abs(y2 - y) - sh/2) {
            if ((x2 - x) < 0) 
                pos = 2
            else pos = 4
        }
    if (is.na(pos)) {
        polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2), 
            y = c(y + sh/2, y + sh/2, y - sh/2, y - sh/2), col = col, 
            border = border)
    }
    else {
        if (pos == 0) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2), 
                y = c(y + sh/2, y + sh/2, y - sh/2, y - sh/2), 
                col = col, border = border)
        if (pos == 1) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x + sw/6, 
                x2, x - sw/6, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y - sh/2, y - sh/2, y2, y - sh/2, y - sh/2), 
                col = col, border = border)
        if (pos == 2) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x - sw/2, 
                x - sw/2, x2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y - sh/2, y - sh/2, y - sh/6, y2, y + sh/6), 
                col = col, border = border)
        if (pos == 3) 
            polygon(x = c(x - sw/2, x - sw/6, x2, x + sw/6, x + 
                sw/2, x + sw/2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y2, y + sh/2, y + sh/2, y - sh/2, y - sh/2), 
                col = col, border = border)
        if (pos == 4) 
            polygon(x = c(x - sw/2, x + sw/2, x + sw/2, x2, x + 
                sw/2, x + sw/2, x - sw/2), y = c(y + sh/2, y + 
                sh/2, y + sh/6, y2, y - sh/6, y - sh/2, y - sh/2), 
                col = col, border = border)
    }
    text(x, y, text, cex = cex, font = font, ...)
    invisible()
}
