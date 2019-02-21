#' Function plotFA
#' 
#' Plots psych::fa object.
#' @param data Numeric data.frame.
#' @details Plots psych::fa object.
#' @keywords plotting
#' @export
#' @examples
#' af.fa(data.frame(x=rnorm(100),y=rnorm(100)+scale(1:100),z=rnorm(100)+scale(1:100)));plotFA(fa)

plotFA <- function (fa = NULL) 
{
    if (is.null(fa)) 
        fa = af.fa(data.frame(x = rnorm(100), y = rnorm(100) + 
            scale(1:100), z = rnorm(100) + scale(1:100)))
    nfact = dim(fa$loadings)[2]
    if (fa$factors == 1) {
        plot(fa)
    }
    else if (fa$factors == 2) {
        plot(fa$loadings[, 1], fa$loadings[, 2], xlab = "factor 1", 
            ylab = "factor 2", xlim = c(-1.1, 1.1), ylim = c(-1.1, 
                1.1), main = paste("Factor loadings", fa$rotation), 
            type = "n")
        lines(cos(seq(0, 2 * pi, length.out = 360)), sin(seq(0, 
            2 * pi, length.out = 360)), col = "grey")
        abline(h = 0, col = "grey")
        abline(v = 0, col = "grey")
        segments(rep(0, dim(fa$loadings)[1]), rep(0, dim(fa$loadings)[1]), 
            fa$loadings[, 1], fa$loadings[, 2])
        points(fa$loadings[, 1], fa$loadings[, 2], pch = 16)
        text(fa$loadings[, 1], fa$loadings[, 2], 1:dim(fa$loadings)[1], 
            pos = 3)
    }
    if (fa$factors > 2) {
        d = data.frame(commonality = round(t(t(fa$communality)), 
            2))
        for (i in 1:dim(fa$loadings)[2]) d = data.frame(d, round((fa$loadings)[, 
            i], 2))
        names(d) = c("commonality", paste("Factor", seq(1:dim(fa$loadings)[2])))
        barplot(abs(t(as.matrix(data.frame(d[, -1])))), legend = c(names(d)[-1]), 
            main = paste("Absolute factor loadings", fa$rotation), 
            names.arg = 1:dim(fa$loadings)[1], xlim = c(0, dim(fa$loadings)[1] * 
                1.2 * 1.5), args.legend = list(x = "topright"))
        abline(h = 0)
        title(sub = paste(nfact, "factors extracted"))
    }
}
