#' Function af.cor
#' 
#' Provides (unweighted or weighted) correlation matrix between variables.
#' @param data Numeric data.frame.
#' @details Provides (unweighted or weighted) correlation matrix between variables. Additionally, p-values and confidence intervals are provided for each pairwise correlation, based on the (potentially weighted) coefficients of correlation and the unweighted sample size.
#' @keywords modeling
#' @export
#' @examples
#' af.cor(dat=sapply(1:10,function(x)rnorm(100)));

af.cor <- function (dat, wei = NULL, use = "pairwise.complete.obs", simplify = F) 
{
    if (is.null(dim(dat))) 
        stop("Please provide a matrix or data.frame as dat")
    if (is.matrix(dat)) 
        if (!is.numeric(dat)) {
            warning("Non-numeric matrix detected. Type-casting applied.")
            dat = apply(dat, 2, as.numeric)
        }
    if (is.data.frame(dat)) 
        if (sum(sapply(dat, is.numeric), na.rm = T) < dim(dat)[2]) {
            warning("Non-numeric matrix detected. Type-casting applied.")
            nam = names(dat[, sapply(dat, is.numeric)])
            dat = as.data.frame(dat[, sapply(dat, is.numeric)])
            names(dat) = nam
        }
    if (is.null(wei)) 
        wei = rep(1, dim(dat)[1])
    dat = as.matrix(dat)
    if (use == "complete.obs") {
        wei = wei[complete.cases(dat)]
        dat = dat[complete.cases(dat), ]
    }
    weightedCorrelation <- function(x, y, w = NULL, cor = T) {
        if (is.null(w)) 
            w = rep(1, length(x))
        stopifnot(length(x) == length(y))
        w <- w/sum(w)
        x <- x - sum(x * w)
        y <- y - sum(y * w)
        vx <- sum(w * x * x)
        vy <- sum(w * y * y)
        vxy <- sum(y * x * w)
        if (cor) 
            vxy = vxy/sqrt(vx * vy)
        return(vxy)
    }
    erg1 = matrix(NA, ncol = dim(dat)[2], nrow = dim(dat)[2])
    erg2 = erg1
    erg3 = erg1
    erg4 = erg1
    erg5 = erg1
    ps <- function(x, y, w, r, co = 0.95) {
        n = length(x)
        df = n - 2
        t = sqrt(df) * r/sqrt(1 - r * r)
        z = atanh(r)
        sig = 1/sqrt(n - 3)
        cint = tanh(z + c(-1, 1) * sig * qnorm((1 + co)/2))
        pval = 2 * min(pt(t, df), pt(t, df, lower.tail = F))
        return(list(pval = pval, lower = min(cint, na.rm = T), 
            upper = max(cint, na.rm = T), n = n))
    }
    for (i in 1:dim(dat)[2]) {
        for (j in 1:dim(dat)[2]) {
            dat_i = as.numeric(dat[, i])
            dat_j = as.numeric(dat[, j])
            wei_i = as.numeric(wei)
            dat_i[is.na(wei_i) | is.na(dat_j)] = NA
            dat_j[is.na(wei_i) | is.na(dat_i)] = NA
            wei_i[is.na(dat_i) | is.na(dat_j)] = NA
            dat_i = dat_i[complete.cases(dat_i)]
            dat_j = dat_j[complete.cases(dat_j)]
            wei_i = wei_i[complete.cases(wei_i)]
            erg1[i, j] = weightedCorrelation(dat_i, rbind(dat_j), 
                wei_i)
            x = ps(dat_i, dat_j, wei_i, erg1[i, j], 0.95)
            erg2[i, j] = x[["pval"]]
            erg3[i, j] = x[["lower"]]
            erg4[i, j] = x[["upper"]]
            erg5[i, j] = x[["n"]]
        }
    }
    rownames(erg1) = colnames(erg1) = colnames(dat)
    rownames(erg2) = colnames(erg2) = colnames(dat)
    rownames(erg3) = colnames(erg3) = colnames(dat)
    rownames(erg4) = colnames(erg4) = colnames(dat)
    rownames(erg5) = colnames(erg5) = colnames(dat)
    erg0 = round(erg1, 2)
    erg2[is.na(erg2)] = 2
    erg0[nchar(erg0) == 3] = paste0(erg0[nchar(erg0) == 3], "0")
    erg0[nchar(erg0) == 1] = paste0(erg0[nchar(erg0) == 1], ".00")
    erg0[erg2 >= 0.05 & erg2 < 0.1] = paste0(erg0[erg2 >= 0.05 & 
        erg2 < 0.1], "'")
    erg0[erg2 < 0.05] = paste0(erg0[erg2 < 0.05], "*")
    erg0[erg2 < 0.01] = paste0(erg0[erg2 < 0.01], "*")
    erg0[erg2 < 0.001] = paste0(erg0[erg2 < 0.001], "*")
    diag(erg0) = "1.00"
    erg2[erg2 == 2] = NA
    if (simplify == T) 
        return(erg = erg1)
    if (simplify == "cor") 
        return(erg = erg0)
    if (simplify == "r") 
        return(erg = erg1)
    if (simplify == "pval") 
        return(erg = erg2)
    if (simplify == "ci.lower") 
        return(erg = erg3)
    if (simplify == "ci.upper") 
        return(erg = erg4)
    if (simplify == "n") 
        return(erg = erg5)
    erg = list(cor = erg0, r = erg1, pval = erg2, ci.lower = erg3, 
        ci.upper = erg4, n = erg5)
    return(erg)
}
