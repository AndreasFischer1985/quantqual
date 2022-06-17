#' Function flowerplot
#' 
#' Plots numeric matrix as a field of flowers.
#' @param x Numeric data.frame or matrix containing the values to be displayed.
#' @param maximum Numeric value representing the maximum value. Only needs to be specified when the data does not contain the theoretically possible maximum.
#' @param rownames Character vector of the same length as x, containing the row names to be displayed. If NULL (default) rownames of x are applied.
#' @param colnames Character vector of the same length as x, containing the column names to be displayed. If NULL (default) colnames of x are applied.
#' @param main Character value, containing the title to be displayed. Defaults to NULL.
#' @param color Character vector, containing the colors of petals. If NULL (default) the rainbow palette is applied.
#' @param color2 Character value, containing the color of the background petals. If NULL, no background petals are plotted. Defaults to "lightgrey".
#' @param add.numbers Logical value specifying whether to draw numbers next to each petal. Defaults to F.
#' @param ndigits Numeric value specifying the number of digits to be plotted (if add.numbers==T). Defaults to 2.
#' @param ncex Size of fonts. If NA (default) is set to cex.
#' @param ncol Vector containing the color of bars. Defaults to "black".
#' @param xshift Numeric value specifying how much to shift flowers to the right. Defaults to 0.
#' @param circle Numeric value specifying the size of the black circle at the center of each flower. Defaults to 0.
#' @param reverseLegend Logical value specifying whether to reverse the order of legend entries. Defaults to F.
#' @details Plots data.frame as a field of flowers. Each column is represented as a separate flower, each row as a flower's petal.
#' @keywords plotting
#' @export
#' @examples
#' flowerplot()

flowerplot <- function (x = NULL, maximum = NULL, rownames = NULL, colnames = NULL, 
    main = NULL, color = NULL, color2 = "lightgrey", add.numbers = F, 
    ncex = 0.8, ndigits = 1, ncol = "black", circle = 0.5, reverseLegend = F, 
    cex = 0.8, cex.legend = 0.8, xlim = NULL, ylim = NULL, dist = 4, 
    legend = NULL, xshift = 0) 
{
    if (!is.null(x)) {
        data = cbind(x)
        data[is.na(data)] = 0
        warning("Input contains missing data.")
    }
    else data = NULL
    if (is.null(data)) 
        data = cbind(option1 = c(1, 1, 1, 1, 1, 1, 1), option2 = c(0.9, 
            0.8, 0.5, 0.9, 1, 0, 0.1), option3 = c(0.8, 0.9, 
            0.8, 0.9, 1, 0.9, 0.7), option4 = c(0.9, 1, 0.7, 
            0.5, 0.5, 0.5, 0.5), option5 = c(0.4, 0.4, 0.4, 1, 
            1, 1, 1))
    x = seq(-2, (dim(data)[2] - 1) * dist + 2, length.out = 2)
    if (!is.numeric(as.matrix(data))) 
        stop("Wrong input. Please provide a numerical matrix.")
    if (is.null(maximum)) 
        maximum = max(data)
    data = data/max(c(data, maximum))
    if (!(dim(data)[1] > 1 & dim(data)[2] > 1)) 
        stop("Wrong input. Please provide a matrix with at least two rows and two columns.")
    if (is.null(rownames(data)) && is.null(rownames)) 
        rownames = paste0("v", 1:dim(data)[1])
    if (!is.null(rownames(data)) && is.null(rownames)) 
        rownames = rownames(data)
    if (is.null(colnames(data)) && is.null(colnames)) 
        colnames = 1:dim(data)[2]
    if (!is.null(colnames(data)) && is.null(colnames)) 
        colnames = colnames(data)
    helleFarbe = rgb(0.9, 0.9, 0.9)
    if (is.null(color)) {
        color = rainbow(dim(data)[1])
    }
    if (length(color) != dim(data)[1]) 
        farben = colorRampPalette(c(helleFarbe, color))(dim(data)[1] + 
            1)
    else farben = color
    petal = function(a = 0.5, b = 1, l = 1, max.l = 7, fl = 1, 
        he = 0, col = farben[l], border = NULL, text = NA) {
        theta <- seq(0, 2 * pi, length = 100)
        x0 = b * sin(seq(0, 2 * pi, length.out = max.l))[l]
        y0 = b * cos(seq(0, 2 * pi, length.out = max.l))[l]
        r = seq(0, 2 * pi, length.out = max.l)[l]
        x = a * b * cos(theta)
        y = b * sin(theta)
        xr = dist * (fl - 1) + xshift + x0 + x * cos(r) + y * 
            sin(r)
        yr = he + y0 - x * sin(r) + y * cos(r)
        polygon(xr, yr, col = col, border = border, xpd = T)
        if (add.numbers == T) 
            if (!is.na(text)) {
                c = b + 0.2
                x0 = c * sin(seq(0, 2 * pi, length.out = max.l))[l]
                y0 = c * cos(seq(0, 2 * pi, length.out = max.l))[l]
                x = a * c * cos(theta)
                y = c * sin(theta)
                xr = dist * (fl - 1) + x0 + x * cos(r) + y * 
                  sin(r)
                yr = he + y0 - x * sin(r) + y * cos(r)
                text(xr[20], yr[20], ifelse(text == 1, "1.0", 
                  ifelse(text == 0, "0.0", round(text, ndigits))), 
                  cex = ncex, col = ncol, xpd = T)
            }
    }
    if (is.null(xlim)) 
        xlim = c(-2 - dist/2, dist * 3/4 + dist * (dim(data)[2])) + 
            dist/2
    if (is.null(ylim)) 
        ylim = c(min(x), max(x))
    plot(x, x, type = "n", xlab = "", ylab = "", axes = FALSE, 
        xlim = xlim, ylim = ylim)
    for (flower1 in 1:dim(data)[2]) {
        height = (max(x) - 2) * (colSums(data)/max(colSums(data)))[flower1]
        segments(dist * (flower1 - 1) + xshift, 0, dist * (flower1 - 
            1) + xshift, height, col = "darkgreen", lwd = 3, 
            xpd = T)
        for (petal1 in 1:dim(data)[1]) {
            petal(0.3, 1, petal1, dim(data)[1] + 1, flower1, 
                height, col = color2[1], border = NA, text = NA)
        }
        for (petal1 in 1:dim(data)[1]) {
            petal(0.3, data[petal1, flower1], petal1, dim(data)[1] + 
                1, flower1, height, text = data[petal1, flower1])
        }
        polygon(dist * (flower1 - 1) + xshift + circle * sin(seq(0, 
            2 * pi, length.out = dim(data)[1] + 1)), height + 
            circle * cos(seq(0, 2 * pi, length.out = dim(data)[1] + 
                1)), col = "black", xpd = T)
        text(dist * (flower1 - 1) + xshift, 0, colnames[flower1], 
            pos = 1, cex = cex, xpd = T)
    }
    segments(-2 + xshift, 0, dist * (dim(data)[2]) + 1 + xshift, 
        0, col = "darkgreen", lwd = 3, xpd = T)
    if (!is.null(main)) 
        title(main)
    legX = dist * 3/4 + dist * (dim(data)[2])
    if (is.null(legend)) 
        if (reverseLegend == T) {
            legend(x = legX - dist, y = legX - dist, legend = c(rownames), 
                fill = farben[1:(dim(data)[1] + 1)], bg = "white", 
                bty = "n", xpd = T, cex = cex.legend)
        }
        else {
            legend(x = legX - dist, y = legX - dist, legend = c(rownames)[length(rownames):1], 
                fill = farben[(dim(data)[1]):1], bg = "white", 
                bty = "n", xpd = T, cex = cex.legend)
        }
    if (!is.null(legend)) 
        legend
    invisible(data)
}
