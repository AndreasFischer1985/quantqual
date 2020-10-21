#' Function textbreaker
#' 
#' Adds linebreaks to text.
#' @param text Character value containing the text to be split into multiple lines.
#' @param maxlength Numeric value specifying the maximum number of characters per line.
#' @details Adds linebreaks to text.
#' @keywords helper
#' @export
#' @examples
#' textbreaker("hello world hello world hello world hello world")

textbreaker <- function (text = "Let's write some text with many different words and plot it in a nice little device.", 
    maxlength = 30, maxlines = 10, centered = F, separator = "\n", 
    trim = T, plot = F, ...) 
{
    text[is.na(text)] = " "
    count = 0
    text = gsub("<br>", " ", text)
    while (nchar(text[length(text)]) > maxlength) {
        count = count + 1
        if (count >= maxlines) 
            break
        spacePos = as.vector(gregexpr("( |\n|\r|/)", text[length(text)])[[1]])
        spacePos = spacePos[spacePos < maxlength]
        spacePos = spacePos[length(spacePos)]
        if (length(spacePos) == 0) {
            spacePos = maxlength
        }
        if (length(spacePos) == 0) 
            break
        text = c(text[-length(text)], substr(text[length(text)], 
            1, spacePos), substr(text[length(text)], spacePos + 
            1, nchar(text[length(text)])))
    }
    devsize = ifelse((length(dev.list()) > 0 | plot == T), list(dev.size()), 
        list(c(7, 6.989583)))[[1]]
    ps = ifelse((length(dev.list()) > 0 | plot == T), list(par()$ps), 
        list(12))[[1]]
    p1 = ifelse((length(dev.list()) > 0 | plot == T), list(par("mai")), 
        list(c(1.02, 0.82, 0.82, 0.42)))[[1]]
    p2 = rep(0, 4)
    if (length(dev.list()) > 0 | plot == T) 
        par(mai = p2)
    th = ps * 1/72
    lspace = 0.52
    size = 1
    fac = (th * lspace * 1/devsize[2] * 6 * size * length(text))
    pos = NULL
    if (!centered) 
        pos = 4
    x = -1 + 0.05
    if (centered) 
        x = 0
    y = mean((length(text):1)/length(text) * fac + (1 - fac) - 
        (1/devsize[2]^2))
    if (centered) 
        y = 0
    lines = length(text)
    if (trim == T) 
        text = paste0(gsub("(^[ \n\r]+|[ \n\r]+$)", "", text), 
            collapse = separator)
    else text = paste0(text, collapse = separator)
    if (plot) {
        plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
        text(x, y, text, pos = pos, ...)
    }
    if (length(dev.list()) > 0 | plot == T) 
        par(mai = p1)
    return(text)
}
