#' Function textbreaker
#' 
#' Adds linebreaks to text.
#' @details Adds linebreaks to text.
#' @keywords helper
#' @export
#' @examples
#' textbreaker("hello world hello world hello world hello world")

textbreaker <- function (text = "Let's write some text with many different words and plot it in a nice little device.", 
    zeilenabstand = 1, maxlength = 30, size = 1, centered = F, 
    separator = "\n     ", plot = F) 
{
    count = 1
    text = gsub("<br>", " ", text)
    while (nchar(text[length(text)]) > maxlength) {
        count = count + 1
        if (count > 10) 
            break
        spacePos = as.vector(gregexpr("( |\n|/)", text[length(text)])[[1]])
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
    p1 = par("mai")
    p2 = p1 * 0
    par(mai = p2)
    th = par()$ps * 1/72
    fac = (th * zeilenabstand * 1/dev.size()[2] * 6 * size * 
        length(text))
    pos = NULL
    if (!centered) 
        pos = 4
    x = 0.05
    if (centered) 
        x = 0.5
    y = (length(text):1)/length(text) * fac + (1 - fac) - (1/dev.size()[2]^2)
    lines = length(text)
    text = paste0(text, collapse = separator)
    y = 0.5
    if (plot) {
        plot(0, 0, type = "n", axes = F)
        text(0, 0, text)
    }
    par(mai = p1)
    return(text)
}
