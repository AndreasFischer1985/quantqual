#' Function plotNNET
#' 
#' Plots the structure of a nnet object.
#' @param net Object of class nnet.
#' @details Plots the structure of a nnet object.
#' @keywords plotting
#' @export
#' @examples
#' library(nnet);net=nnet(y~.,data=data.frame(y=rnorm(100),x=rnorm(100)),size=3,trace=F);plotNNET(net)

plotNNET <- function (net = NULL, cex = 4, xlim = c(0, 4), xnames = NULL, 
    ynames = NULL) 
{
    require(nnet)
    if (is.null(net)) 
        net = nnet(y ~ ., data = data.frame(y = rnorm(100), x = rnorm(100)), 
            size = 3, trace = F)
    if (sum(grepl("skip", deparse(net$call))) == T) 
        warning("parameter 'skip' will be ignored")
    inp = net$n[1] + 1
    hid = net$n[2] + 1
    out = net$n[3]
    ma = max(c(inp, hid, out))
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    plot(seq(0, 3, length.out = 2), seq(0, ma + 1, length.out = 2), 
        type = "n", xlim = xlim, xlab = "", ylab = "", axes = F)
    y1 = rep(y1, hid)
    y2 = rep(y2, each = inp)
    segments(1, y1[((inp + 1):(hid * inp))], 2, y2[((inp + 1):(hid * 
        inp))])
    segments(2, y2, 3, y3)
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    points(rep(1, length(y1) - 1), y1[-1], pch = 16, col = "green", 
        cex = cex)
    points(rep(2, length(y2) - 1), y2[-1], pch = 16, col = "yellow", 
        cex = cex)
    points(rep(3, length(y3)), y3, pch = 16, col = "red", cex = cex)
    points(c(1, 2), c(y1[1], y2[1]), pch = 16, col = "orange", 
        cex = cex)
    points(rep(1, length(y1) - 1), y1[-1], pch = 1, col = "black", 
        cex = cex)
    points(rep(2, length(y2) - 1), y2[-1], pch = 1, col = "black", 
        cex = cex)
    points(rep(3, length(y3)), y3, pch = 1, col = "black", cex = cex)
    points(c(1, 2), c(y1[1], y2[1]), pch = 1, col = "black", 
        cex = cex)
    title(paste0((inp - 1), "-", (hid - 1), "-", out, "-Structure"))
    if (inp - 1 == length(xnames)) 
        text(1 - 0.2, y1[-1], xnames, pos = 2)
    if (out == length(ynames)) 
        text(3 + 0.2, y3, ynames, pos = 4)
}
