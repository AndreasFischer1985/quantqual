#' Function plotMAT
#' 
#' Plots a numeric matrix as one line per row.
#' @param matrix Numeric matrix containing the values to be displayed.
#' @param main Character value representing the barplot's title. Defaults to "Cumulation over time"
#' @param xlab Character value representing the x-axis label. Defaults to "time"
#' @param main Character value representing the y-axis label. Defaults to "cumulated sum"
#' @param xlim.factor Numeric value for adding extra space to the right of the barplot. Defaults to 1.
#' @param cumsum Logical value indicating whether the cumsum of each row in the matrix of each row should be plotted. Defaults to T.
#' @param show.legend Logical value indicating whether a legend should be drawn instead of texts. Defaults to T.
#' @param add.shadow Logical value indicating whether lines should be surrounded ba a black line. Defaults to F.
#' @param add.grid Logical value indicating whether a grid should be drawn. Defaults to F.
#' @param col Vector containing the color of bars. If NULL (default) colors are generated based on the rainbow-palette.
#' @param cex Size of fonts. Defaults to .7.
#' @param manual.addon Numeric vector containing verical adjustments.
#' @details Plots a numeric matrix as one line per row. By default cumsum of each row is plotted.
#' @keywords plotting
#' @export
#' @examples
#' plotMAT()

plotMAT <- function (matrix = NULL, main = "Cumulation over Time", xlab = "", 
    ylab = "cumulated sum", xlim.factor = 1.5, cumsum = T, show.legend = T, 
    add.shadow = F, add.grid = T, col = NULL, cex = 0.7, manual.addon = 0) 
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
    if (is.null(col)) 
        col = rainbow(dim(cs)[1])
    plot(1:dim(cs)[2], seq(0, max(cs, na.rm = T), length.out = dim(cs)[2]), 
        type = "n", ylim = c(min(cs), max(cs)), xlim = c(1, dim(cs)[2] * 
            xlim.factor), xaxt = "n", xlab = xlab, ylab = ylab, 
        main = main)
    if (add.grid) 
        grid()
    for (i in 1:dim(cs)[1]) {
        if (add.shadow) 
            lines(1:dim(cs)[2], cs[i, ], col = "black", lwd = 4)
        lines(1:dim(cs)[2], cs[i, ], col = col[i], lwd = 2)
    }
    axis(1, at = 1:dim(cs)[2], labels = rep(NA, dim(cs)[2]), 
        cex.axis = 0.5)
    x0 = cs
    x0 = min(x0) - 0.08 * (max(x0) - min(x0))
    text((1:dim(cs)[2]) + (0.02 * dim(cs)[2]), x0, colnames(cs), 
        pos = 2, srt = 45, xpd = T, cex = 0.5)
    order = order(cs[, dim(cs)[2]], decreasing = T)
    cs = cs[order, ]
    if (!show.legend) {
        text(dim(cs)[2] + 1, cs[, dim(cs)[2]] + manual.addon, 
            rownames(cs), pos = 4, cex = cex)
        points(x = rep(dim(cs)[2] + 1, dim(cs)[1]), y = cs[, 
            dim(cs)[2]] + manual.addon, col = rainbow(dim(cs)[1])[order(cs[, 
            dim(cs)[2]], decreasing = T)], pch = 15)
        points(x = rep(dim(cs)[2] + 1, dim(cs)[1]), y = cs[, 
            dim(cs)[2]] + manual.addon, col = "black", pch = 0)
    }
    if (show.legend) 
        legend("right", rownames(cs), fill = col[order], cex = cex)
}
