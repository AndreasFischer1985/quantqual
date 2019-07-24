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
    color = par("fg"), gcolor = par("fg"), lcolor = "gray", xlim = range(x[is.finite(x)]), 
    main = NULL, xlab = NULL, ylab = NULL, deviations = NA, x2 = NA, 
    vertLine = 0, horizLines = T, max = 40, sort = F, ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
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
    if (is.null(x)) {
        x = rnorm(100)
        names(x) = paste0("Var", 1:100)
    }
    if (length(x) > max) {
        if (!is.na(deviations[1])) 
            deviations = deviations[c(1:floor(max/2), (length(x) - 
                floor(max/2)):length(x))]
        if (!is.na(x2[1])) 
            x2 = x2[c(1:floor(max/2), (length(x) - floor(max/2)):length(x))]
        x = x[c(1:floor(max/2), (length(x) - floor(max/2)):length(x))]
        warning(paste("Only", max, "values are displayed"))
    }
    plot.new()
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
        points(x2, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
    }
    points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
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
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    if (!is.na(vertLine)) 
        abline(v = vertLine, col = color)
    invisible()
}
