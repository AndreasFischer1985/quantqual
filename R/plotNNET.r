#' Function plotNNET
#' 
#' Plots the structure of a nnet object.
#' @param net Object of class nnet.
#' @details Plots the structure of a nnet object.
#' @keywords plotting
#' @export
#' @examples
#' library(nnet);net=nnet(y~.,data=data.frame(y=rnorm(100),x=rnorm(100)),size=3,trace=F);plotNNET(net)

plotNNET <- function (net = NULL, cex = 0.8, cex2 = 4, xlim = c(0, 4), xnames = NULL, 
    ynames = NULL, digits = 2) 
{
    if (is.null(net)) 
        net = nnet::nnet(y ~ ., data = data.frame(y = rnorm(100), 
            x = rnorm(100)), size = 3, trace = F)
    if (net$n[3] > 1) 
        stop("Currently nnet.objects with more than one output unit are not supported")
    if (sum(grepl("skip", deparse(net$call))) == T) 
        warning("parameter 'skip' will be ignored")
    wts <- net$wts
    wm <- c("bias", paste("input", seq_len(net$n[1]), sep = ""))
    if (net$n[2] > 0L) 
        wm <- c(wm, paste("hidden", seq_len(net$n[2]), sep = ""))
    if (net$n[3] > 1) 
        wm <- c(wm, paste("output", seq_len(net$n[3]), sep = ""))
    else wm <- c(wm, "out")
    names(wts) <- apply(cbind(wm[1 + net$conn], wm[1 + rep(1:net$nunits - 
        1, diff(net$nconn))]), 1, function(x) paste(x, collapse = "->"))
    inp = net$n[1] + 1
    hid = net$n[2] + 1
    out = net$n[3]
    ma = max(c(inp, hid, out))
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    y0 = c(y1, y2[-1], y3, y2[1])
    from = net$conn + 1
    from[from == 1 & 1:length(from) > (net$n[1] + 1) * net$n[2]] = length(y0)
    from = y0[from]
    to = y0[1 + rep(1:net$nunits - 1, diff(net$nconn))]
    dat = data.frame(wts, y1 = from, y2 = to, row.names = names(wts))
    plot(seq(0, 3, length.out = 2), seq(0, ma + 1, length.out = 2), 
        type = "n", xlim = xlim, xlab = "", ylab = "", axes = F)
    y1 = rep(y1, hid)
    y2 = rep(y2, each = inp)
    segments(1, y1[((inp + 1):(hid * inp))], 2, y2[((inp + 1):(hid * 
        inp))])
    segments(2, y2, 3, y3)
    if (!is.null(digits)) {
        findpos = function(y1, y2, x1 = 0, x2 = 1, p = 0.7) {
            xd = 1
            yd = abs(y2 - y1)
            xd = x2 - x1
            yd2 = (p * xd) * yd/xd + min(y1, y2)
        }
        for (i in 1:dim(dat)[1]) boxedText(x = ifelse(i > (net$n[1] + 
            1) * net$n[2], 2.5, 1.5), y = (findpos(dat[i, "y1"], 
            dat[i, "y2"])), text = round(dat[i, "wts"], digits), 
            col = "white", border = NA, vspace = 1.5, hspace = 1.2, 
            cex = cex)
    }
    y1 = (1:inp) + (ma - inp)/2
    y2 = (1:hid) + (ma - hid)/2
    y3 = (1:out) + (ma - out)/2
    points(rep(1, length(y1) - 1), y1[-1], pch = 16, col = "green", 
        cex = cex2)
    points(rep(2, length(y2) - 1), y2[-1], pch = 16, col = "yellow", 
        cex = cex2)
    points(rep(3, length(y3)), y3, pch = 16, col = "red", cex = cex2)
    points(c(1, 2), c(y1[1], y2[1]), pch = 16, col = "orange", 
        cex = cex2)
    points(rep(1, length(y1) - 1), y1[-1], pch = 1, col = "black", 
        cex = cex2)
    points(rep(2, length(y2) - 1), y2[-1], pch = 1, col = "black", 
        cex = cex2)
    points(rep(3, length(y3)), y3, pch = 1, col = "black", cex = cex2)
    points(c(1, 2), c(y1[1], y2[1]), pch = 1, col = "black", 
        cex = cex2)
    title(paste0((inp - 1), "-", (hid - 1), "-", out, "-Structure"))
    if (inp - 1 == length(xnames)) 
        text(1 - 0.2, y1[-1], xnames, pos = 2)
    if (out == length(ynames)) 
        text(3 + 0.2, y3, ynames, pos = 4)
    invisible(wts)
}
