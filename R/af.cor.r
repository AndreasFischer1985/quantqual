#' Function af.cor
#' 
#' Provides (unweighted or weighted) correlation matrix between variables.
#' @param data Numeric data.frame.
#' @details Provides (unweighted or weighted) correlation matrix between variables.
#' @keywords modeling
#' @export
#' @examples
#' af.cor(dat=sapply(1:10,function(x)rnorm(100)));

af.cor <- function (dat, wei = NULL, use = "pairwise.complete.obs") 
{
    if (is.null(dim(dat))) 
        stop("Please provide a matrix or data.frame as dat")
    if (is.null(wei)) 
        wei = rep(1, dim(dat)[1])
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
    erg = matrix(NA, ncol = dim(dat)[2], nrow = dim(dat)[2])
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
            erg[i, j] = weightedCorrelation(dat_i, rbind(dat_j), 
                wei_i)
        }
    }
    rownames(erg) = colnames(erg) = names(dat)
    erg
}
