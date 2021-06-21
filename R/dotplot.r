#' Function dotplot
#' 
#' Plots a modified dotchart.
#' @param x Numeric vector.
#' @param vertLine Logical value specifying whether to plot a vertical line at x=0. Defaults to T.
#' @param horizLines Logical value specifying whether to plot horizontal lines between each point and the y axis. Defaults to T.
#' @param max Numeric value indicating the maximum number of frequencies to be displayed. Defaults to 40.
#' @param main Character value specifying the plot's title. Defaults to "Comparison of Word Frequencies"
#' @param pch Numeric element speccifying the symbol to represent points. Defaults to 16
#' @param cex Numeric value indicating the text size. Defaults to .7.
#' @details Plots vector as modified dotchart.
#' @keywords plotting
#' @export
#' @examples
#' dotplot(rnorm(100))

dotplot <- function (x = NULL, labels = NULL, groups = NULL, gdata = NULL, 
    cex = par("cex"), pt.cex = cex, pch = 16, gpch = 21, bg = par("bg"), 
    col = NULL, color = par("fg"), gcolor = par("fg"), lcolor = "gray", 
    xlim = range(x[is.finite(x)]), main = NULL, xlab = NULL, 
    ylab = NULL, deviations = NA, x2 = NA, vertLine = 0, horizLines = T, 
    max = 40, sort = F, add.numbers = F, ndigits = 2, ncex = NA, 
    nsrt = 0, npos = NA, ncol = "black", main1 = NULL, main2 = NULL, 
    main3 = NULL, adj.main1 = 0, adj.main2 = 0, adj.main3 = 0, 
    col.main1 = "black", col.main2 = "black", col.main3 = "black", 
    cex.main1 = 1.2, cex.main2 = 1.2, cex.main3 = 1.2, font.main1 = 1, 
    font.main2 = 2, font.main3 = 4, omitZeros = F, xaxs = "i", 
    yaxs = "i", ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    if (is.null(x)) {
        x = rnorm(100)
        names(x) = paste0("Var", 1:100)
    }
    if (is.null(dim(x)[2])) 
        if (length(x) > max) {
            if (!is.na(deviations[1])) 
                deviations = deviations[c(1:floor(max/2), (length(x) - 
                  floor(max/2)):length(x))]
            if (!is.na(x2[1])) 
                x2 = x2[c(1:floor(max/2), (length(x) - floor(max/2)):length(x))]
            x = x[c(1:floor(max/2), (length(x) - floor(max/2)):length(x))]
            warning(paste("Only", max, "values are displayed"))
        }
    n <- length(x)
    if (sort == T) {
        if (!is.na(deviations[1])) 
            if (is.matrix(x)) 
                deviations = deviations[order(x[, 1]), ]
            else deviations = deviations[order(x)]
        if (!is.na(x2[1])) 
            if (is.matrix(x)) 
                x2 = x2[order(x[, 1]), ]
            else x2 = x2[order(x)]
        if (is.matrix(x)) 
            x = x[order(x[, 1]), ]
        else x = x[order(x)]
    }
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(1L:nrow(x))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
    }
    addChars = ifelse(is.character(add.numbers), add.numbers, 
        "")
    if (is.character(add.numbers)) 
        add.numbers = T
    if (is.null(ncex)) 
        ncex = cex
    else if (is.na(ncex)) 
        ncex = cex
    if (!is.null(npos)) 
        if (is.na(npos)) 
            npos = 4
    round2 = function(x, ndigits, addChars = "") {
        x2 = as.character(round(x, digits = ndigits))
        x2[is.na(x)] = ""
        if (omitZeros == T) 
            x2[x2 == 0] = ""
        x2 = paste0(x2, addChars)
        x2[x2 == addChars] = ""
        return(x2)
    }
    plot.new()
    if (!is.null(xaxs)) 
        par(xaxs = xaxs)
    if (!is.null(yaxs)) 
        par(yaxs = yaxs)
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        if (!is.na(x2[1])) 
            x2 <- x2[o]
        if (!is.na(deviations[1])) 
            deviations <- deviations[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    if (horizLines & is.na(x2[1]) & is.na(deviations[1])) {
        xlim[1] = min(0, xlim[1])
        xlim[2] = max(0, xlim[2])
        segments(x0 = 0, y0 = y, x1 = x, y1 = y, col = color)
    }
    if (!is.na(deviations[1])) {
        segments(x0 = x + deviations, y0 = y, x1 = x - deviations, 
            y1 = y, col = color)
    }
    if (!is.na(x2[1])) {
        horizLines = F
        segments(x0 = x, y0 = y, x1 = x2, y1 = y, col = color)
        points(x2, y, pch = pch, col = ifelse(is.null(col), list(color), 
            list(col))[[1]], bg = bg, cex = pt.cex/cex)
    }
    points(x, y, pch = pch, col = ifelse(is.null(col), list(color), 
        list(col))[[1]], bg = bg, cex = pt.cex/cex)
    if (add.numbers) 
        text(x, y, round2(x, ndigits, addChars), pos = npos, 
            col = ncol, cex = ncex, srt = nsrt, xpd = T)
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
            text(gdata, gpos, "test")
        }
    }
    axis(1)
    box()
    if (!is.null(main)) 
        title(main = main, xlab = xlab, ylab = ylab, ...)
    if (!is.null(main1)) 
        title(main1, line = 1, adj = adj.main1, cex.main = cex.main1, 
            col = col.main1, font.main = font.main1)
    if (!is.null(main2)) 
        title(main2, line = 2, adj = adj.main2, cex.main = cex.main2, 
            col = col.main2, font.main = font.main2)
    if (!is.null(main3)) 
        title(main3, line = 3, adj = adj.main3, cex.main = cex.main3, 
            col = col.main3, font.main = font.main3)
    if (!is.na(vertLine)) 
        abline(v = vertLine, col = color)
    invisible()
}
