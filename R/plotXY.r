#' Function plotXY
#' 
#' Plots bivariate correlation based on two numeric vectors.
#' @param x Numeric vector.
#' @param y Numeric vector of the same length as x.
#' @param na.rm Logical value indicating whether missing values should be skipped. Defaults to T.
#' @param color1 Color of points in the scattergram. Defaults to rgb(0/255,84/255,122/255,.7).
#' @param color2 Color of the regression line. Defaults to "darkorange".
#' @param show.text Logical value indicating whether text should be added to the regression line. Defaults to T.
#' @details Plots scattergram and bivariate correlation based on two numeric vectors.
#' @keywords plotting
#' @export
#' @examples
#' plotXY()

plotXY <- function (x = NULL, y = NULL, na.rm = T, color1 = rgb(0/255, 
    84/255, 122/255, 0.7), color2 = "darkorange", show.text = T, 
    xlab = "x", ylab = "y", main = "Correlation", pch = 16, ...) 
{
    if (is.null(x) & is.null(x)) {
        x = rnorm(100)
        y = rnorm(100)
    }
    data = data.frame(x, y)
    if (na.rm == T) 
        data = data[complete.cases(data), ]
    l1 = lm(data[, 2] ~ data[, 1])
    len = 100
    in1 = seq(min(data[, 1]), max(data[, 1]), length.out = len)
    co1 = confint(l1)
    ou1 = in1 * coef(l1)[2] + coef(l1)[1]
    ou2 = data.frame(in1 * co1[2, 1] + co1[1, 1], in1 * co1[2, 
        2] + co1[1, 2], in1 * co1[2, 1] + co1[1, 2], in1 * co1[2, 
        2] + co1[1, 1])
    outer = apply(ou2, 1, max)
    inner = apply(ou2, 1, min)
    plot(data[, 1], data[, 2], xlab = xlab, ylab = ylab, pch = pch, 
        col = color1[1], main = main)
    title(sub = "Dashed lines represent 95%-confidence interval.", 
        cex.sub = 0.7)
    lines(in1, ou1, col = color2[1], lwd = 2)
    lines(in1, outer, lty = 2, col = rgb(106/255, 160/255, 186/255))
    lines(in1, inner, lty = 2, col = rgb(106/255, 160/255, 186/255))
    if (show.text) 
        text(in1[length(in1)], ou1[length(ou1)] - (abs(ou1[length(ou1)]) + 
            abs(inner[length(inner)]))/2, paste0("r=", round(cor(data)[2], 
            2), "\n(p=", round(coef(summary(l1))[2, "Pr(>|t|)"], 
            2), "; df=", (summary(l1)$df[2]), ")"), cex = 0.7, 
            col = ifelse(coef(summary(l1))[2, "Pr(>|t|)"] < 0.05, 
                "black", "grey"), pos = 2)
    return(summary(l1))
}
