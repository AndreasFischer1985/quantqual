#' Function bp
#' 
#' Custom barplot with labeled bars.
#' @param x Numeric vector, matrix or data.frame containing the values to be displayed.
#' @param sd Numeric vector, matrix or data.frame of same format as x containing standard deviations.
#' @param cex Size of axis fonts. Defaults to .7.
#' @param beside Logical value indicating if bars should be placed next to each other. Defaults to T.
#' @param horiz Logical value indicating if bars should be placed horizontal. Defaults to F.
#' @param add.numbers Logical value indicating if numbers should be placed above bars. Defaults to F.
#' @param ndigits Numeric value specifying the number of digits to be plotted (if add.numbers==T). Defaults to 2.
#' @param ncex Size of fonts. If NA (default) is set to cex.
#' @param nsrt Numeric value specifying the rotation of the numbers (between 0 and 360 degrees). Defaults to 0.
#' @param npos Numeric value specifying the position of numbers. If NA (default) it is determined automatically.
#' @param ncol Vector containing the color of bars. Defaults to "black".
#' @param grid Logical value indicating whether to plot a grid. Defaults to T.
#' @param plot Logical value indicating whether to plot the barplot. Defaults to T.
#' @param main Character vector with one element containing the barplot's title. Defaults to NULL
#' @param xlim.factor Numeric value for adding extra space to the right of the barplot. Defaults to 1.
#' @param xlim Numeric vector containing xlim.
#' @param las Numeric value specifying the rotation of the y-axis (0 for 90 percent rotation, 1 for 0 percent rotation). Defaults to 1.
#' @param srt Numeric value specifying the rotation of the x-axis (between 0 and 360 degrees). Defaults to 45.
#' @param names.arg Character vector containing names of bars. If NULL (default) colnames of x will be applied as names.arg.
#' @param legend.text Legend text. Set to NA or to F to supress legend. If NULL (default) rownames of x will be applied as legend.text.
#' @param args.legend List with arguments to pass to legend(). Defaults to list(bg = "white").
#' @param density Vector giving the density of shading lines, in lines per inch, for the bars or bar components. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.
#' @param angle Slope of shading lines, given as an angle in degrees (counter-clockwise), for the bars or bar components.
#' @param col Vector containing the color of bars. If NULL (default) colors are generated based on the rainbow-palette.
#' @param grid.col Character or rgb value containing the color of grid lines. Defaults to rgb(0,0,0,.1).
#' @param axes Logical value indicating whether to plot axes. Defaults to T.
#' @param add Logical value indicating whether to plot the barplot to the current device. Defaults to F.
#' @param adj Numeric value between 0 and 1, indicating where to plot the title between left (0) and right (1). Defaults to 0.
#' @param default.labels Logical value, indicating whether to use traditional barplot-labels (which, e.g., hide in case of overlap) instead of bp-style labels. Defaults to F.
#' @param ... Additional graphical parameters for barplot.
#' @details Plots a bar and adds labels and numbers to bars. Optionally allows for plotting standard deviations around each bar.
#' @keywords plotting
#' @export
#' @examples
#' bp(data.frame(group1=c(variable1=1,variable2=2),group2=c(variable1=3,variable2=4)),horiz=T,main="Example")

bp <- function (x = NULL, sd = NULL, cex = 0.7, beside = T, horiz = F, 
    add.numbers = F, ndigits = 2, ncex = NA, nsrt = 0, npos = NA, 
    ncol = "black", grid = T, plot = T, main = NULL, xlim.factor = 1.5, 
    las = 1, srt = 45, xlim = NULL, names.arg = NULL, legend.text = NULL, 
    args.legend = list(bg = "white"), density = NULL, angle = 45, 
    col = NULL, grid.col = rgb(0, 0, 0, 0.1), axes = T, add = F, 
    adj = 0, default.labels = F, ...) 
{
    if (is.na(ncex) | is.null(ncex)) 
        ncex = cex
    if (!is.null(npos)) 
        if (is.na(npos)) 
            if (beside == T) 
                npos = ifelse(horiz == T, 4, 3)
            else npos = NULL
    if (is.null(x)) 
        x = data.frame(test1 = c(1, 2, 3), test2 = c(2, 3, 4))
    else if (is.table(x)) {
        y = matrix(sapply(x, as.numeric), nrow = dim(x)[1])
        if (length(dim(x)) == 1) {
            rownames(y) = names(x)
        }
        else {
            rownames(y) = rownames(x)
            colnames(y) = colnames(x)
        }
        x = y
    }
    el = list(...)
    x = as.matrix(x)
    x2 = ifelse(dim(x)[2] == 1, list(x[, 1]), list(x))[[1]]
    if (!is.null(sd)) {
        sd = ifelse(dim(x)[2] == 1, list(as.matrix(sd)[, 1]), 
            list(as.matrix(sd)))[[1]]
    }
    if (is.null(rownames(x))) 
        rownames(x) = 1:dim(x)[1]
    if (is.null(colnames(x))) 
        colnames(x) = 1:dim(x)[2]
    if (is.matrix(x2)) {
        rownames(x2) = rownames(x)
        colnames(x2) = colnames(x)
    }
    else names(x2) = rownames(x)
    width = unlist(ifelse(length(el[["width"]]) > 0, el["width"], 
        list(1))[[1]])
    space = unlist(ifelse(length(el[["space"]]) > 0, el["space"], 
        ifelse(beside & dim(x)[2] > 1, list(c(0, 1)), list(0.2)))[[1]])
    if (dim(x)[2] == 1) 
        beside = T
    if (is.null(col)) 
        col = rainbow(dim(x)[1])
    if (is.null(legend.text) & dim(x)[2] > 1) 
        legend.text = rownames(x)
    if (is.null(xlim)) 
        if (!horiz) {
            b = barplot(x2, beside = beside, horiz = horiz, plot = F, 
                add = add, ...)
            xlim = c(min(b) - (width[1]/2), max(b) + (width[1]/2))
            if (!is.null(legend.text)) 
                xlim[2] = xlim[2] * xlim.factor
        }
        else {
            if (beside) {
                xlim = c(min(c(min(x), 0)), max(c(max(x), 0)))
            }
            else {
                xlim = c(min(c(min(colSums(x)), 0)), max(c(max(colSums(x)), 
                  0)))
            }
            if (!is.null(legend.text)) 
                xlim[2] = xlim[2] * xlim.factor
        }
    if (sum(is.na(legend.text)) > 0) 
        legend.text = NULL
    else if (length(legend.text) == 1) 
        if (legend.text[[1]] == F) 
            legend.text = NULL
    if (!is.null(names.arg)) {
        if (is.na(names.arg) & length(names.arg) == 1) 
            names.arg = rep(NA, dim(x)[2])
        if (dim(x)[2] == 1) 
            rownames(x) = names.arg
        else colnames(x) = names.arg
    }
    else names.arg = ifelse(dim(x)[2] == 1, list(rownames(x)), 
        list(colnames(x)))[[1]]
    b = as.matrix(barplot(x2, names.arg = ifelse(default.labels == 
        T, list(names.arg), list(rep("", dim(x)[2])))[[1]], beside = beside, 
        horiz = horiz, density = density, angle = angle, col = col, 
        xlim = xlim, main = main, plot = plot, las = las, axes = axes, 
        add = add, adj = adj, ...))
    if (!is.null(sd) & beside) 
        if (!horiz) {
            arrows(b, x2 + sd, b, x2 - sd, angle = 90, code = 3, 
                length = 0.1, xpd = T)
        }
        else {
            arrows(x2 + sd, b, x2 - sd, b, angle = 90, code = 3, 
                length = 0.1, xpd = T)
        }
    if (beside == T) {
        rownames(b) = rownames(x)
        colnames(b) = colnames(x)
    }
    else {
        rownames(b) = colnames(x)
    }
    b2 = ifelse(dim(b)[2] == 1, list(t(b)), list(as.matrix(b)))[[1]]
    if (!is.null(el[["ylim"]])) {
        x0 = data.frame(rep(min(el[["ylim"]]), dim(x)[1]), rep(max(el[["ylim"]])/dim(x)[1], 
            dim(x)[1]))
    }
    else {
        x0 = data.frame(rep(0, dim(x)[1]), x)
    }
    if (beside) {
        x0 = min(x0, na.rm = T) - 0.05 * (max(x0, na.rm = T) - 
            min(x0, na.rm = T))
    }
    else {
        x0 = min(colSums(x0, na.rm = T)) - 0.05 * (max(colSums(x0, 
            na.rm = T)) - min(colSums(x0, na.rm = T)))
    }
    abline2 = function(xl, xlf, ...) {
        xaxp <- par("xaxp")
        yaxp <- par("yaxp")
        segments(x0 = min(xl), y0 = seq(yaxp[1], yaxp[2], (yaxp[2] - 
            yaxp[1])/yaxp[3]), x1 = max(xl)/xlf, y1 = seq(yaxp[1], 
            yaxp[2], (yaxp[2] - yaxp[1])/yaxp[3]), ...)
    }
    abline3 = function(xl, xlf, ...) {
        xaxp <- par("xaxp")
        yaxp <- par("yaxp")
        segments(y0 = min(xl), x0 = seq(xaxp[1], xaxp[2], (xaxp[2] - 
            xaxp[1])/xaxp[3]), y1 = max(xl)/xlf, x1 = seq(xaxp[1], 
            xaxp[2], (xaxp[2] - xaxp[1])/xaxp[3]), ...)
    }
    if (plot != F) 
        if (!horiz) {
            if (grid != F) 
                abline2(xlim, ifelse(!is.null(legend.text), xlim.factor, 
                  1), col = grid.col)
            if (axes != F & default.labels != T) 
                text(colMeans(b2), 0 + x0, colnames(b2), srt = srt, 
                  pos = ifelse((srt == 0 | srt == 180 | srt == 
                    360), 1, 2), xpd = T, cex = cex)
            if (add.numbers) 
                if (beside) {
                  text(b, x, paste0(round(x, ndigits)), pos = npos, 
                    col = ncol, cex = ncex, srt = nsrt, xpd = T)
                }
                else {
                  text(b, x[1, ]/2, paste0(round(x[1, ], ndigits)), 
                    col = ncol, xpd = T, cex = ncex, srt = nsrt, 
                    pos = npos)
                  if (dim(x)[1] > 1) 
                    for (i in 2:dim(x)[1]) {
                      m = as.matrix(x[c(1:i), ])
                      dim(m) = c(length(c(1:i)), dim(x)[2])
                      text(b, (colSums(rbind(m[1:i, ])) + colSums(rbind(m[1:(i - 
                        1), ])))/2, labels = paste0(round(x[i, 
                        ], ndigits)), col = ncol, xpd = T, cex = ncex, 
                        srt = nsrt, pos = npos)
                    }
                }
        }
        else {
            if (grid != F) 
                abline3(par("usr")[1:2], 1, col = grid.col)
            if (axes != F & default.labels != T) 
                text(0 + x0, colMeans(b2), colnames(b2), srt = srt - 
                  45, pos = 2, xpd = T, cex = cex)
            if (add.numbers) 
                if (beside) {
                  text(x, b, paste0(round(x, ndigits)), pos = npos, 
                    col = ncol, cex = ncex, srt = nsrt, xpd = T)
                }
                else {
                  text(x[1, ]/2, b, paste0(round(x[1, ], ndigits)), 
                    col = ncol, xpd = T, cex = ncex, srt = nsrt, 
                    pos = npos)
                  if (dim(x)[1] > 1) 
                    for (i in 2:dim(x)[1]) {
                      m = as.matrix(x[c(1:i), ])
                      dim(m) = c(length(c(1:i)), dim(x)[2])
                      text((colSums(rbind(m[1:i, ])) + colSums(rbind(m[1:(i - 
                        1), ])))/2, b, labels = paste0(round(x[i, 
                        ], ndigits)), col = ncol, xpd = T, cex = ncex, 
                        srt = nsrt, pos = npos)
                    }
                }
        }
    xy <- par("usr")
    if (!is.null(legend.text)) 
        if (is.null(args.legend)) {
            legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), 
                legend = legend.text, angle = angle, density = density, 
                fill = col, xjust = 1, yjust = 1)
        }
        else {
            args.legend1 <- list(x = xy[2L] - xinch(0.1), y = xy[4L] - 
                yinch(0.1), legend = legend.text, angle = angle, 
                density = density, fill = col, xjust = 1, yjust = 1)
            args.legend1[names(args.legend)] <- args.legend
            do.call("legend", args.legend1)
        }
    return(b)
}
