#' Function plotXY
#' 
#' Plots bivariate correlation based on two numeric vectors.
#' @param x Numeric vector.
#' @param y Numeric vector of the same length as x.
#' @param complexity Numeric value specifying the amount of nonlinearity modelled. Defaults to 0 (i.e., a linear model).
#' @param rep.nnet Numeric value specifying the number of nnet-objects to choose the best model from.
#' @param attrModel Logical value specifying whether to add the model as an attribute to the object returned.
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

plotXY <- function (x = NULL, y = NULL, complexity = 0, rep.nnet = 10, 
    attrModel = T, na.rm = T, color1 = rgb(0, 0, 0, 0.7), color2 = rgb(0, 
        0, 1), color3 = rgb(0, 0, 1, 0.2), xlab = "x", ylab = "y", 
    axes = T, add = F, main = NA, sub = NA, pch = 16, lwd = 2, 
    cex = 0.7, cex.sub = 0.7, generalize = F, main1 = NULL, main2 = NULL, 
    main3 = NULL, mar = NA, adj.main1 = 0, adj.main2 = 0, adj.main3 = 0, 
    col.main1 = "black", col.main2 = "black", col.main3 = "black", 
    cex.main1 = 1.2, cex.main2 = 1.2, cex.main3 = 1.2, font.main1 = 1, 
    font.main2 = 2, font.main3 = 4, ...) 
{
    if (is.null(sub)) 
        sub = ifelse(complexity == 0, "Shaded area represents 95%-confidence interval.", 
            "Shaded area represents 95%-prediction interval.")
    if (is.null(x) & is.null(y)) {
        x = rnorm(100)
        y = rnorm(100)
    }
    mar0 = NULL
    if (is.numeric(mar)) {
        mar0 = par("mar")
        par(mar = mar)
    }
    data = data.frame(x, y)
    if (na.rm == T) 
        data = data[complete.cases(data), ]
    data0 = data
    data = data.frame(scale(data))
    colnames(data) = colnames(data0)
    nnet = NULL
    lm = NULL
    if (complexity > 0) 
        if (length(grep("^quantqual$", (installed.packages()[, 
            "Package"]))) == 0) {
            complexity = 0
            warning("complexity set to 0 because quantqual-package is not installed.\nYou may install it via devtools::install_github(\"AndreasFischer1985/quantqual\")")
        }
    if (complexity > 0) {
        if (!generalize) 
            nnet = quantqual::nnets(data, "y", size = complexity, 
                linout = T, rep.nnet = rep.nnet)[[1]]
        else nnet = quantqual::af.nnet(data, "y", size = complexity, 
            decay = NULL, linout = T, rep.nnet = rep.nnet)
        xTrain = data[colnames(data) != "y"]
        yTrain = data["y"]
        p = quantqual::predintNNET(nnet, xTrain, yTrain, main = main, 
            sub = sub, color1 = color1, color2 = color2, color3 = color3, 
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
        lm = l1
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
    in1 = unscale(in1, mean(data0[, 1], na.rm = T), sd(data0[, 
        1], na.rm = T))
    ou1 = unscale(ou1, mean(data0[, 2], na.rm = T), sd(data0[, 
        2], na.rm = T))
    inner = unscale(inner, mean(data0[, 2], na.rm = T), sd(data0[, 
        2], na.rm = T))
    outer = unscale(outer, mean(data0[, 2], na.rm = T), sd(data0[, 
        2], na.rm = T))
    if (add == F) 
        plot(data0[, 1], data0[, 2], xlab = xlab, ylab = ylab, 
            main = main, type = "n", axes = axes, ...)
    if (add == F) 
        if (!is.null(sub)) 
            title(sub = sub, cex.sub = cex.sub)
    polygon(c(in1, in1[length(in1):1]), c(inner, outer[length(outer):1]), 
        col = color3[1], border = NA)
    if (length(data0[, 1]) != length(color1)) 
        color1 = color1[1]
    points(data0[, 1], data0[, 2], pch = pch, col = color1)
    lines(in1, ou1, , col = color2[1], lwd = lwd)
    dat = data.frame(predictor = in1, prediction = ou1, lower.bound = inner, 
        upper.bound = outer)
    if (attrModel) 
        if (!is.null(nnet)) 
            attr(dat, "model") = nnet
        else attr(dat, "model") = lm
    if (!is.null(main1)) 
        title(main1, line = 1, adj = adj.main1, cex.main = cex.main1, 
            col = col.main1, font.main = font.main1)
    if (!is.null(main2)) 
        title(main2, line = 2, adj = adj.main2, cex.main = cex.main2, 
            col = col.main2, font.main = font.main2)
    if (!is.null(main3)) 
        title(main3, line = 3, adj = adj.main3, cex.main = cex.main3, 
            col = col.main3, font.main = font.main3)
    if (is.numeric(mar)) 
        par(mar = mar0)
    return(invisible(dat))
}
