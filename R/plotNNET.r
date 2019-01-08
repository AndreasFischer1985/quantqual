#' Function plotNNET
#' 
#' Plots the structure of a nnet object.
#' @param net Object of class nnet.
#' @details Plots the structure of a nnet object.
#' @keywords plotting
#' @export
#' @examples
#' library(nnet);net=nnet(y~.,data=data.frame(y=rnorm(100),x=rnorm(100)),size=3,trace=F);plotNNET(net)

plotNNET <- function (net = NULL) 
{
    require(nnet)
    if (is.null(net)) 
        net = nnet(y ~ ., data = data.frame(y = rnorm(100), x = rnorm(100)), 
            size = 3, trace = F)
    inp = net$n[1] + 1
    hid = net$n[2] + 1
    out = net$n[3]
    ma = max(c(inp, hid, out))
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    plot(seq(0, 3, length.out = 2), seq(0, ma + 1, length.out = 2), 
        type = "n", xlab = "", ylab = "", axes = F)
    y1 = rep(y1, hid)
    y2 = rep(y2, each = inp)
    segments(1, y1[((inp + 1):(hid * inp))], 2, y2[((inp + 1):(hid * 
        inp))])
    segments(2, y2, 3, y3)
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    points(rep(1, length(y1) - 1), y1[-1], pch = 16, col = "green")
    points(rep(2, length(y2) - 1), y2[-1], pch = 16, col = "yellow")
    points(rep(3, length(y3)), y3, pch = 16, col = "red")
    points(c(1, 2), c(y1[1], y2[1]), pch = 16, col = "orange")
    points(rep(1, length(y1) - 1), y1[-1], pch = 1, col = "black")
    points(rep(2, length(y2) - 1), y2[-1], pch = 1, col = "black")
    points(rep(3, length(y3)), y3, pch = 1, col = "black")
    points(c(1, 2), c(y1[1], y2[1]), pch = 1, col = "black")
    title(paste0((inp - 1), "-", (hid - 1), "-", out, "-Structure"))
}
