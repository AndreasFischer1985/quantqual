#' Function packedBubbleChart
#' 
#' Plots vector as packed bubble chart.
#' @param vec Numeric vector, potentially with names.
#' @param main Character vector with one element containing the plot's title. Defaults to NULL
#' @param show.text Logical value indicating whether bubbles should be labelled with names(vec). Defaults to T.
#' @param beak.names Logical value indicating whether line breaks should be added after each whitespace. Defaults to F.
#' @param beak.names Logical value indicating whether values of vec should be added to vec's names before labelling the bubbles.
#' @param cex Numeric value indicating the text size. Defaults to .8.
#' @param a Numeric value multiplied with normalizd vec. Defaults to 90.
#' @param b Numeric value added to normalized vec*a. Defaults to 10.
#' @param col Character vector indicating the bubbles' color. If NULL (default) the rainbow palette is applied to vec's ranks.
#' @details Plots vector as packed bubble chart. Returns coordinates and radius of each bubble
#' @keywords plotting
#' @export
#' @examples
#' packedBubbleChart()

packedBubbleChart <- function (vec = NULL, main = NULL, show.text = T, break.names = F, 
    add.vec = T, cex = 0.8, col = NULL, cutoff = 2, a = 90, b = 10) 
{
    parmar = par()$mar
    par(mar = parmar - min(parmar))
    if (is.null(vec)) {
        vec = c(`Kretzschmar\n&\nSüß\n2015` = 16, `Güß\net. al.\n2015` = 12, 
            `Fischer\n&\nNeubert\n2105` = 12, `Editorial\n2015` = 9, 
            `Hundertmark\net al.\n2015` = 9, `Dutt\n2015` = 7, 
            `Engelhart\net al.\n2017` = 6, `Wendt\n2017` = 5, 
            `Gonzalez\net al.\n2016` = 4, `Vangsnes\net al.\n2017` = 3, 
            `Editorial\net al.\n2016` = 3, `Frank &\nKluge\n2017` = 2, 
            `Sharma\net al.\n2017` = 2, `Rohe\nwt al.\n2016` = 2)
        if (is.null(main)) 
            main = "Citations 2018"
    }
    if (is.null(names(vec))) 
        names(vec) = vec
    if (!show.text) 
        names(vec) = 1:length(vec)
    if (break.names) 
        names(vec) = gsub(" ", "\n", names(vec))
    if (add.vec) 
        names(vec) = paste0(vec, " x\n", names(vec))
    vec = (((vec - min(vec))/(max(vec) - min(vec))))
    vec = sort(vec * a + b, decreasing = T)
    coord = packcircles::circleProgressiveLayout(vec)
    plot(c(min(coord[, 1]) - max(coord[, 3]), max(coord[, 1]) + 
        max(coord[, 3])), c(min(coord[, 2]) - max(coord[, 3]), 
        max(coord[, 2]) + max(coord[, 3])), type = "n", xlab = "", 
        ylab = "", axes = F, main = main)
    circle = function(x, y, r = 1, c = "black") polygon(x + r * 
        sin(seq(0, 2 * pi, length.out = 100)), y + r * cos(seq(0, 
        2 * pi, length.out = 100)), col = c)
    if (is.null(col)) 
        col = rainbow(max(rank(vec)))[max(rank(vec)) + 1 - round(rank(vec))]
    for (i in 1:dim(coord)[1]) {
        circle(coord[i, 1], coord[i, 2], coord[i, 3], col[i])
        text(coord[i, 1], coord[i, 2], names(vec)[i], cex = ifelse(coord[i, 
            3] < cutoff, cex * 0.7, cex))
    }
    par(mar = parmar)
    return(coord)
}
