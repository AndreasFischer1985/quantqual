#' Function bp
#' 
#' Custom barplot with labeled bars.
#' @param x Numeric vector, matrix or data.frame containing the values to be displayed.
#' @param sd Numeric vector, matrix or data.frame of same format as x containing standard deviations.
#' @param cex Size of fonts. Defaults to .7.
#' @param beside Logical value indicating if bars should be placed next to each other. Defaults to T.
#' @param horiz Logical value indicating if bars should be placed horizontal. Defaults to F.
#' @param add.numbers Logical value indicating if numbers should be placed above bars. Defaults to Fs.
#' @param plot Logical value indicating whether to plot the barplot. Defaults to T.
#' @param main Character vector with one element containing the barplot's title. Defaults to NULL
#' @param xlim.factor Numeric value for adding extra space to the right of the barplot. Defaults to 1.
#' @param xlim Numeric vector containing xlim.
#' @param names.arg Character vector containing names of bars. If NULL (default) colnames of x will be applied as names.arg.
#' @param legend.text Legend text. If NULL (default) rownames of x will be applied as legend.text.
#' @param col Vector containing the color of bars. If NULL (default) colors are generated based on the rainbow-palette.
#' @param ... Additional graphical parameters for barplot.
#' @details Plots a bar and adds labels and numbers to bars. Optionally allows for plotting standard deviations around each bar.
#' @keywords plotting
#' @export
#' @examples
#' bp(data.frame(group1=c(variable1=1,variable2=2),group2=c(variable1=3,variable2=4)),horiz=T,main="Example")

bp <- function (x = NULL, sd = NULL, cex = 0.7, beside = T, horiz = F, 
    add.numbers = F, plot = T, main = NULL, xlim.factor = 1.5, 
    xlim = NULL, names.arg = NULL, legend.text = NULL, col = NULL, 
    ...) 
{
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
    width = ifelse(length(el[["width"]]) > 0, el["width"], list(1))[[1]]
    space = ifelse(length(el[["space"]]) > 0, el["space"], ifelse(beside & 
        dim(x)[2] > 1, list(c(0, 1)), list(0.2)))[[1]]
    if (dim(x)[2] == 1) 
        beside = T
    if (!is.null(names.arg)) 
        colnames(x) = names.arg
    if (is.null(col)) 
        col = rainbow(dim(x)[1])
    if (is.null(legend.text) & dim(x)[2] > 1) 
        legend.text = rownames(x)
    if (is.null(xlim)) 
        if (!horiz) {
            b = barplot(x2, beside = beside, horiz = horiz, plot = F, 
                ...)
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
    b = as.matrix(barplot(x2, names.arg = rep("", dim(x)[2]), 
        beside = beside, horiz = horiz, col = col, xlim = xlim, 
        legend.text = legend.text, main = main, plot = plot, 
        ...))
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
    if (!horiz) {
        text(colMeans(b2), 0 + x0, paste0(colnames(b2), ""), 
            srt = 45, pos = 2, xpd = T, cex = cex)
        if (add.numbers) 
            if (beside) {
                text(b, x, paste0(round(x, 1)), pos = 3, col = "black", 
                  cex = cex, xpd = T)
            }
            else {
                text(b, x[1, ], ifelse(x[1, ] > 1, paste0(round(x[1, 
                  ], 1)), ""), pos = 1, col = "black", cex = cex, 
                  xpd = T)
                if (dim(x)[1] > 1) 
                  for (i in 2:dim(x)[1]) {
                    m = as.matrix(x[c(1:i), ])
                    dim(m) = c(length(c(1:i)), dim(x)[2])
                    text(b, colSums(m), labels = paste0(round(x[i, 
                      ], 1)), pos = 1, col = "black", cex = cex, 
                      xpd = T)
                  }
            }
    }
    else {
        text(0 + x0, colMeans(b2), paste0(colnames(b2), ""), 
            srt = 0, pos = 2, xpd = T, cex = cex)
        if (add.numbers) 
            if (beside) {
                text(x, b, paste0(round(x, 1)), pos = 4, col = "black", 
                  cex = cex, xpd = T)
            }
            else {
                text(x[1, ], b, ifelse(x[1, ] > 1, paste0(round(x[1, 
                  ], 1)), ""), pos = 2, col = "black", cex = cex, 
                  xpd = T)
                if (dim(x)[1] > 1) 
                  for (i in 2:dim(x)[1]) {
                    m = as.matrix(x[c(1:i), ])
                    dim(m) = c(length(c(1:i)), dim(x)[2])
                    text(colSums(m), b, labels = paste0(round(x[i, 
                      ], 1)), pos = 2, col = "black", cex = cex, 
                      xpd = T)
                  }
            }
    }
    return(b)
}
