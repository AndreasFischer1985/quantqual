#' Function distrib
#' 
#' Returns and plots a vetor's unweighted/weighted distribution.
#' @param dat Numeric vector.
#' @param wei Numeric vector representing of the same length as dat. Represents the weight that is attributed to each element of dat.
#' @param kat Factor of the same length as dat. Specifies subgroups of dat to summarize.
#' @param plot Logical value specifying whether to plot the weighted data. Defaults to T.
#' @param plot.o Logical value specifying whether to plot the original data. Defaults to F.
#' @param simplify Logical value specifying whether to return simplified data. Defaults to T.
#' @param mar Numeric vector specifying margins. Defaults to NULL.
#' @param width Numeric value specifying the width of the plot. Defaults to NULL.
#' @param height Numeric value specifying the width of the plot. Defaults to NULL.
#' @details Returns and plots a vetor's unweighted/weighted distribution. 
#' @keywords modeling
#' @export
#' @examples
#' distrib(1:10)

distrib <- function (dat, wei = NULL, kat = NULL, plot = T, plot.o = F, 
    simplify = T, mar = NULL, width = NULL, height = NULL, main2 = "Boxplot", 
    main1 = "", round = T, ...) 
{
    if (is.null(kat)) 
        kat = rep(1, length(dat))
    if (is.null(wei)) 
        wei = rep(1, length(dat))
    dat = as.numeric(dat)
    wei = as.numeric(wei)
    dat[is.na(kat) | is.na(wei)] = NA
    wei[is.na(kat) | is.na(dat)] = NA
    kat[is.na(dat) | is.na(wei)] = NA
    lev = levels(as.factor(kat))
    erg = lapply(lev, function(x) {
        w.tab = sapply(levels(as.factor(dat[kat == x])), function(y) {
            w.n = sum(wei[kat == x & dat == y], na.rm = T)
        })
        if (round == T & length(w.tab) > 0) 
            w.dat = as.numeric(rep(names(w.tab), round(w.tab)))
        else w.dat = as.numeric(rep(names(w.tab), w.tab))
        list(w.statistics = c(w.n = sum(wei[kat == x], na.rm = T), 
            w.sum = sum(dat[kat == x] * wei[kat == x], na.rm = T), 
            w.mean = sum(dat[kat == x] * wei[kat == x], na.rm = T)/sum(wei[kat == 
                x], na.rm = T), w.median = median(w.dat, na.rm = T), 
            w.sd = sd(w.dat, na.rm = T), w.min = min(w.dat, na.rm = T), 
            w.max = max(w.dat, na.rm = T), w.sum2 = sum(w.dat, 
                na.rm = T), w.mean2 = mean(w.dat, na.rm = T)), 
            w.table = w.tab, w.data = w.dat, o.statistics = c(o.n = sum(kat == 
                x, na.rm = T), o.sum = sum(dat[kat == x], na.rm = T), 
                o.mean = sum(dat[kat == x], na.rm = T)/sum(kat == 
                  x, na.rm = T), o.median = median(dat[kat == 
                  x], na.rm = T), o.sd = sd(dat[kat == x], na.rm = T), 
                o.min = min(dat[kat == x], na.rm = T), o.max = max(dat[kat == 
                  x], na.rm = T), o.sum2 = sum(dat[kat == x], 
                  na.rm = T), o.mean2 = mean(dat[kat == x], na.rm = T)), 
            o.table = table(dat[kat == x], useNA = "always"), 
            o.data = as.numeric(na.omit(dat[kat == x])))
    })
    names(erg) = lev
    if (plot == T) {
        cols2 = function(col, transparency = 0.3, opaque = T) {
            if (opaque == T) 
                res = sapply(as.data.frame(col2rgb(col)), function(x) colorRampPalette(c(rgb(x[1]/255, 
                  x[2]/255, x[3]/255), "white"))(3)[2])
            else res = apply(col2rgb(col), 2, function(x) rgb(x[1]/255, 
                x[2]/255, x[3]/255, transparency))
            return(res)
        }
        x = lapply(erg, function(x) x[["w.data"]])
        m = sapply(erg, function(x) x[["w.statistics"]]["w.mean"])
        x2 = lapply(erg, function(x) x[["o.data"]])
        m2 = sapply(erg, function(x) x[["o.statistics"]]["o.mean"])
        x2 = x2[order(m)]
        m2 = m2[order(m)]
        x = x[order(m)]
        m = m[order(m)]
        if (!is.null(width) & !is.null(height)) 
            dev.new(width = width, height = height)
        if (!is.null(mar)) 
            p0 = par("mar")
        if (!is.null(mar)) 
            par(mar = mar)
        if (plot.o == T) {
            boxplot(x, at = 1:length(x), horizontal = T, axes = F, 
                col = rgb(127/255, 169/255, 188/255), range = 0, 
                whiskcol = "grey", staplecol = "grey", whisklty = 1, 
                boxwex = 0.5/length(x), ...)
            boxplot(x2, at = (1:length(x)) + 0.25, horizontal = T, 
                axes = F, col = cols2(rgb(127/255, 169/255, 188/255), 
                  opaque = T), range = 0, whiskcol = "grey", 
                staplecol = "grey", whisklty = 1, add = T, boxwex = 0.5/length(x), 
                ...)
            points(m[1:length(x)], 1:length(x), pch = 16, col = "black")
            points(m2[1:length(x2)], (1:length(x2)) + 0.25, pch = 16, 
                col = "black")
            print(m)
            print(m2)
        }
        else {
            boxplot(x, at = 1:length(x), horizontal = T, axes = F, 
                col = rgb(127/255, 169/255, 188/255), range = 0, 
                whiskcol = "grey", staplecol = "grey", whisklty = 1, 
                ...)
            points(m[1:length(x)], 1:length(x), pch = 16, col = "black")
        }
        axis(1, col = NA, col.ticks = "grey")
        axis(2, at = 1:length(x), labels = names(x), las = 2, 
            col = NA, col.ticks = NA)
        if (!is.null(mar)) 
            par(mar = p0)
        title(main2, line = 2, adj = 0)
        title(main1, line = 1, adj = 0, font.main = 1)
    }
    if (plot == F) {
        if (sum(wei != 1, na.rm = T) > 0) {
            if (simplify) 
                return(rbind(sapply(erg, function(x) x[["w.statistics"]]), 
                  sapply(erg, function(x) x[["o.statistics"]])))
            else return(erg)
        }
        else if (simplify) 
            return(sapply(erg, function(x) x[["o.statistics"]]))
        else return(erg)
    }
    else if (sum(wei != 1, na.rm = T) > 0) {
        if (simplify) 
            return(invisible(rbind(sapply(erg, function(x) x[["w.statistics"]]), 
                sapply(erg, function(x) x[["o.statistics"]]))))
        else return(invisible(erg))
    }
    else if (simplify) 
        return(invisible(sapply(erg, function(x) x[["o.statistics"]])))
    else return(invisible(erg))
}
