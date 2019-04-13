#' Function plotMAT
#' 
#' Plots a numeric matrix as one line per row.
#' @param matrix Numeric matrix containing the values to be displayed.
#' @param main Character value representing the barplot's title. If NULL (default) it's set to "Cumulation over time"
#' @param xlab Character value representing the x-axis label. If NULL it's set to "time". Defaults to ""
#' @param ylab Character value representing the y-axis label. If NULL (default) it's set to "cumulated sum" if cumsum==T, and to "value" otherwise.
#' @param lwd Numeric vector specifying line width. Defaults to 2.
#' @param lty Numeric vector specifying line type. Defaults to 1.
#' @param pch Numeric vector specifying points type. Defaults to NULL.
#' @param type Numeric vector specifying plot-type of lines. Defaults to "l".
#' @param main Character value representing the title. If NULL (default) it's set to "Cumulation over Time" if cumsum==T, and to "Development over Time" otherwise.
#' @param xlim Numeric vector with two elements. If NULL (default) xlim is detemined automatically.
#' @param ylim Numeric vector with two elements. If NULL (default) ylim is detemined automatically.
#' @param xlim.factor Numeric value for adding extra space to the right of the plot. Defaults to 1.
#' @param las Numeric value specifying the rotation of the y-axis (0 for 90 percent rotation, 1 for 0 percent rotation). Defaults to 1.
#' @param srt Numeric value specifying the rotation of the x-axis (between 0 and 360 degrees). Defaults to 45.
#' @param cumsum Logical value indicating whether the cumsum of each row in the matrix of each row should be plotted. Defaults to T.
#' @param show.legend Logical value indicating whether a legend should be drawn instead of texts. Defaults to T.
#' @param add Logical value indicating whether to draw lines to an existing plot. Defaults to F.
#' @param add.shadow Logical value indicating whether lines should be surrounded ba a black line. Defaults to F.
#' @param grid Logical value indicating whether a grid should be drawn. Defaults to T.
#' @param col Vector containing each line's color. If NULL (default) colors are generated based on the rainbow-palette.
#' @param cex Relative size of legend font. Defaults to .7.
#' @param cex.axis1 Relative size of x-axis font. Defaults to .7.
#' @param cex.axis2 Relative size of y-axis font. Defaults to .7.
#' @param frame Relative size of invisible frame around fonts. Defaults to 1.
#' @param manual.addon Numeric vector containing verical adjustments.
#' @details Plots a numeric matrix as one line per row. By default cumsum of each row is plotted.
#' @keywords plotting
#' @export
#' @examples
#' plotMAT()

plotMAT <- function (matrix = NULL, main = NULL, xlab = "", ylab = NULL, 
    lwd = 2, lty = 1, pch = NULL, type = "l", xlim = NULL, ylim = NULL, 
    xlim.factor = 1.5, las = 1, srt = 45, cumsum = T, show.legend = F, 
    add = F, add.shadow = F, grid = T, col = NULL, cex = 0.7, 
    cex.axis1 = 0.5, cex.axis2 = 0.7, frame = 1, manual.addon = NULL) 
{
    if (is.null(matrix)) {
        matrix = t(data.frame(`ID 15455/20157` = c(32, 254, 22, 
            54, 35, 30, 46, 245, 10, 6, 17, 24, 16, 14, 16, 22, 
            36, 192, 89, 133, 14, 5, 11, 13, 18, 5), `ID 34608/32684` = c(NA, 
            NA, NA, NA, NA, NA, 83, 76, 37, 26, 17, 29, 29, 30, 
            14, 24, 9, 20, 9, 19, 23, 11, 15, 17, 13, 4), `ID 17663/17669` = c(153, 
            175, 12, 38, 5, 24, 33, 6, 44, 108, 33, 31, 6, 25, 
            15, 12, 13, 11, 15, 12, 8, 10, 7, 9, 6, 5), `ID 28510/27028` = c(NA, 
            174, 28, 30, 22, 34, 47, 22, 20, 16, 14, 23, 23, 
            7, 22, 20, 34, 226, 99, 156, 13, 13, 13, 31, 19, 
            17), `ID 43102/40459` = c(NA, NA, NA, NA, NA, NA, 
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 62, 70, 
            51, 49, 34, 64, 84, 87, 62), `ID 33651/27352` = c(NA, 
            33, 3, 6, 7, 10, 3, 2, 3, 0, 4, 7, 4, 4, 6, 2, 1, 
            3, 3, 4, 4, 1, 1, 0, 1, 2), `ID 29308/26102` = c(NA, 
            94, 5, 8, 6, 8, 13, 13, 7, 6, 10, 12, 8, 7, 13, 12, 
            2, 12, 6, 6, 14, 7, 8, 5, 5, 6), `ID 43868/37385` = c(NA, 
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, 
            26, 9, 3, 8, 3, 6, 11, 2, 4, 2, 2, 2), `ID 37687/35162` = c(NA, 
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 39, 23, 19, 
            21, 39, 50, 241, 90, 128, 15, 13, 18, 10, 24, 17), 
            `ID 23807/17668` = c(130, 173, 8, 9, 1, 2, 11, 2, 
                56, 1, 11, 14, 11, 14, 9, 5, 4, 5, 11, 9, 6, 
                3, 6, 4, 5, 5), `ID 13131/19021` = c(102, 241, 
                17, 23, 14, 4, 20, 5, 11, 3, 9, 15, 9, 3, 9, 
                9, 2, 15, 11, 7, 10, 5, 7, 7, 9, 7), `ID 26416/21633` = c(12, 
                142, 5, 7, 10, 2, 24, 9, 5, 6, 10, 9, 8, 5, 14, 
                5, 1, 17, 4, 7, 8, 2, 5, 3, 1, 4), `ID 40004/35160` = c(NA, 
                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 30, 11, 
                18, 20, 15, 17, 17, 17, 12, 24, 11, 21, 5, 17, 
                11), `ID 33724/29549` = c(NA, NA, NA, NA, 53, 
                26, 38, 27, 24, 16, 22, 24, 30, 32, 25, 25, 28, 
                27, 21, 19, 11, 7, 12, 19, 7, 16), `ID 28995/22724` = c(NA, 
                184, 15, 14, 21, 9, 18, 3, 8, 1, 7, 10, 9, 5, 
                13, 7, 2, 3, 3, 7, 5, 2, 4, 4, 1, 5), `ID 23945/22084` = c(12, 
                447, 22, 17, 12, 11, 23, 4, 10, 5, 18, 14, 11, 
                9, 11, 4, 18, 13, 17, 19, 21, 20, 20, 6, 8, 9), 
            `ID 41543/37287` = c(NA, NA, NA, NA, NA, NA, NA, 
                NA, NA, NA, NA, NA, NA, 55, 27, 20, 24, 32, 8, 
                13, 13, 8, 11, 11, 20, 5), `ID 33346/29578` = c(NA, 
                NA, NA, 12, 44, 23, 12, 6, 11, 2, 10, 15, 17, 
                4, 6, 4, 4, 6, 5, 7, 6, 3, 5, 1, 3, 4), row.names = c("2015", 
                "2016", "Jan 2017", "Feb 2017", "Mar 2017", "Apr 2017", 
                "May 2017", "Jun 2017", "Jul 2017", "Aug 2017", 
                "Sep 2017", "Oct 2017", "Nov 2017", "Dec 2017", 
                "Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", 
                "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", 
                "Sep 2018", "Oct 2018", "Nov 2018", "Dec 2018")))
    }
    if (is.null(xlab)) 
        xlab = "time"
    if (is.null(ylab)) 
        ylab = ifelse(cumsum, "cumulated sum", "value")
    if (is.null(ylab)) 
        ylab = ifelse(cumsum, "Cumulation over Time", "Development over Time")
    if (!is.null(manual.addon)) 
        if (length(manual.addon) != dim(matrix)[1]/2) 
            manual.addon = rep(0, dim(matrix)[1]/2)
    cs = matrix
    if (cumsum) 
        cs = t(apply(matrix, 1, function(x) {
            x[is.na(x)] = 0
            return(cumsum(x))
        }))
    if (cumsum) 
        cs = cs[order(cs[, dim(cs)[2]], decreasing = T), ]
    if (!cumsum) 
        cs = cs[order(rowSums(cs, na.rm = T), decreasing = T), 
            ]
    if (is.null(rownames(cs))) 
        rownames(cs) = 1:dim(cs)[1]
    if (is.null(colnames(cs))) 
        colnames(cs) = 1:dim(cs)[2]
    if (is.null(col)) 
        col = rainbow(dim(cs)[1])
    if (is.null(ylim)) 
        ylim = c(min(cs, na.rm = T), max(cs, na.rm = T))
    if (is.null(xlim)) 
        xlim = c(1, dim(cs)[2] * xlim.factor)
    if (is.null(lwd)) 
        lwd = 1
    if (length(lwd) != dim(cs)[1]) 
        lwd = rep(lwd[1], dim(cs)[1])
    if (is.null(lty)) 
        lty = 1
    if (length(lty) != dim(cs)[1]) 
        lty = rep(lty[1], dim(cs)[1])
    if (is.null(pch)) 
        pch = 15
    if (length(pch) != dim(cs)[1]) 
        pch = rep(pch[1], dim(cs)[1])
    if (is.null(type)) 
        type = "l"
    if (length(type) != dim(cs)[1]) 
        type = rep(type[1], dim(cs)[1])
    if (!add) 
        plot(1:dim(cs)[2], seq(0, max(cs, na.rm = T), length.out = dim(cs)[2]), 
            type = "n", ylim = ylim, xlim = xlim, xaxt = "n", 
            xlab = xlab, ylab = ylab, main = main, las = las, 
            cex.axis = cex.axis2)
    if (grid) {
        abline2 = function(cs, ...) {
            xaxp <- par("xaxp")
            yaxp <- par("yaxp")
            segments(x0 = 0, y0 = seq(yaxp[1], yaxp[2], (yaxp[2] - 
                yaxp[1])/yaxp[3]), x1 = dim(cs)[2], y1 = seq(yaxp[1], 
                yaxp[2], (yaxp[2] - yaxp[1])/yaxp[3]), ...)
        }
        abline2(cs, col = rgb(0, 0, 0, 0.1))
    }
    for (i in 1:dim(cs)[1]) {
        if (add.shadow) 
            lines(1:dim(cs)[2], cs[i, ], col = "black", lwd = lwd[i] + 
                2, lty = lty[i], type = type[i], pch = pch[i])
        lines(1:dim(cs)[2], cs[i, ], col = col[i], lwd = lwd[i], 
            lty = lty[i], type = type[i], pch = pch[i])
    }
    axis(1, at = 1:dim(cs)[2], labels = rep(NA, dim(cs)[2]), 
        cex.axis = cex.axis1, las)
    x0 = cs
    x0 = min(x0) - 0.08 * (max(x0) - min(x0))
    text((1:dim(cs)[2]) + (0.02 * dim(cs)[2]), x0, colnames(cs), 
        pos = 2, srt = srt, xpd = T, cex = cex.axis1)
    order = order(cs[, dim(cs)[2]], decreasing = T)
    cs = cs[order, ]
    if (!is.null(manual.addon) & !show.legend) {
        text(dim(cs)[2] + 1, cs[, dim(cs)[2]] + manual.addon, 
            rownames(cs), pos = 4, cex = cex)
        points(x = rep(dim(cs)[2] + 1, dim(cs)[1]), y = cs[, 
            dim(cs)[2]] + manual.addon, col = col[order], pch = 15)
        points(x = rep(dim(cs)[2] + 1, dim(cs)[1]), y = cs[, 
            dim(cs)[2]] + manual.addon, col = "black", pch = 0)
    }
    else if (!show.legend) {
        x1 = rep(dim(cs)[2] + 1, dim(cs)[1])
        y1 = cs[, dim(cs)[2]]
        text1 = rownames(cs)
        d = quantqual::decollide(x1, y1, text1, cex = cex, verbose = F, 
            frame = frame, lock.x = T)
        x1 = as.numeric(d[, 1])
        y1 = as.numeric(d[, 2])
        text1 = d[, 3]
        text(x1, y1, text1, pos = 4, cex = cex)
        points(x = x1, y = y1, col = col[order], pch = 15)
        points(x = x1, y = y1, col = "black", pch = 0)
    }
    if (show.legend) 
        legend("right", rownames(cs), fill = col[order], cex = cex)
}
