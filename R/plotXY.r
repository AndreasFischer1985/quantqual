#' Function plotXY
#' 
#' Plots bivariate correlation based on two numeric vectors.
#' @param x Numeric vector.
#' @param y Numeric vector of the same length as x.
#' @param na.rm Logical value indicating whether missing values should be skipped. Defaults to T.
#' @param color1 Color of points in the scattergram. Defaults to rgb(0,0,0,.7).
#' @param color2 Color of the regression line. Defaults to rgb(0,0,1).
#' @param color3 Color of the prediction interval. Defaults to rgb(0,0,1,.2).
#' @param ... additional parameters passed to the plot function.
#' @details Plots scattergram and bivariate correlation based on two numeric vectors.
#' @keywords plotting
#' @export
#' @examples
#' plotXY()

plotXY <- function (x = NULL, y = NULL, complexity = 1, na.rm = T, color1 = rgb(0, 
    0, 0, 0.7), color2 = rgb(0, 0, 1), color3 = rgb(0, 0, 1, 
    0.2), xlab = "x", ylab = "y", axes = T, add = F, main = "Bivariate Relation", 
    sub = "Shaded area represents 95%-prediction interval.", 
    pch = 16, lwd = 2, cex = 0.7, cex.sub = 0.7, generalize = F, 
    ...) 
{
    if (is.null(x) & is.null(y)) {
        x = rnorm(100)
        y = rnorm(100)
    }
    data = data.frame(x, y)
    if (na.rm == T) 
        data = data[complete.cases(data), ]
    data = data.frame(scale(data))
    colnames(data) = colnames(data0)
    if (complexity > 0) {
        nnet = NULL
        if (!generalize) 
            nnet = nnet::nnet(y ~ ., data = data, size = complexity, 
                linout = T)
        else nnet = af.nnet(data, "y", size = NULL, linout = T)
        xTrain = data[colnames(data) != "y"]
        yTrain = data["y"]
        p = predintNNET(nnet, xTrain, yTrain, main = main, sub = sub, 
            color1 = color1, color2 = color2, color3 = color3, 
            xlab = xlab, ylab = ylab, axes = axes, plot = F)
        p = p[order(data[, 1]), ]
        len = dim(p)[1]
        in1 = sort(data[, 1])
        ou1 = p[, 1]
        inner = p[, 2]
        outer = p[, 3]
    }
    else {
        l1 = lm(data[, 2] ~ data[, 1])
        co1 = confint(l1)
        len = 100
        in1 = seq(min(data[, 1]), max(data[, 1]), length.out = len)
        ou1 = in1 * coef(l1)[2] + coef(l1)[1]
        ou2 = data.frame(in1 * co1[2, 1] + co1[1, 1], in1 * co1[2, 
            2] + co1[1, 2], in1 * co1[2, 1] + co1[1, 2], in1 * 
            co1[2, 2] + co1[1, 1])
        inner = apply(ou2, 1, min)
        outer = apply(ou2, 1, max)
    }
    unscale = function(x, m, s) x * s + m
    in1 = unscale(in1, mean(x, na.rm = T), sd(x, na.rm = T))
    ou1 = unscale(ou1, mean(y, na.rm = T), sd(y, na.rm = T))
    inner = unscale(inner, mean(y, na.rm = T), sd(y, na.rm = T))
    outer = unscale(outer, mean(y, na.rm = T), sd(y, na.rm = T))
    if (add == F) 
        plot(x, y, xlab = xlab, ylab = ylab, main = main, type = "n", 
            axes = axes, ...)
    if (add == F) 
        if (!is.null(sub)) 
            title(sub = sub, cex.sub = cex.sub)
    polygon(c(in1, in1[length(in1):1]), c(inner, outer[length(outer):1]), 
        col = color3[1], border = NA)
    points(x, y, pch = pch, col = color1[1])
    lines(in1, ou1, , col = color2[1], lwd = lwd)
    return(data.frame(predictor = in1, prediction = ou1, lower.bound = inner, 
        upper.bound = outer))
}
