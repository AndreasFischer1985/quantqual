#' Function prepareDF
#' 
#' Scales numeric data and dummy codes factors, omits outliers and missing values.
#' @param data data.frame containing data.
#' @param output index or name of criterion in data.
#' @param out.omit logical value, specifying whether outliers (scale(data)>3) shoud be set to NA.
#' @param na.omit logical value, specifying whether NA shoud be omitted.
#' @keywords preprocessing
#' @export
#' @examples
#' data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100));
#' prepareDF(data,"y1")

prepareDF <- function (data, output = NULL, scale = T, out.omit = T, na.omit = T) 
{
    data = data.frame(data)
    if (is.null(names(data))) 
        names(data) = 1:dim(data)[2]
    if (is.numeric(output)) 
        output = ifelse(output > dim(data)[2], 1, output)
    if (is.character(output)) 
        output = ifelse(length(grep(output, names(data))) == 
            0, 1, which(names(data) == output)[1])
    if (!is.null(output)) 
        names(data)[output] = "output"
    if (is.null(output)) 
        names(data)[1] = "output"
    data = data.frame(data)
    if (scale) 
        data0 = data.frame(scale(data[!sapply(data, function(x) is.factor(x))]))
    else data0 = data.frame(data[!sapply(data, function(x) is.factor(x))])
    data1 = data[sapply(data, function(x) is.factor(x))]
    if (dim(data1)[2]) 
        dummy = function(x, g = NULL, na.rm = TRUE) {
            n <- length(x)
            t <- table(x)
            l <- length(t)
            if (is.null(g)) {
                u <- matrix(0, nrow = n, ncol = l)
                if (na.rm) {
                  u[is.na(x), ] <- NA
                }
                xlev <- as.factor(x)
                for (i in 1:n) {
                  u[i, xlev[i]] <- 1
                }
                colnames(u) <- names(t)
            }
            else {
                u <- rep(0, n)
                xl <- as.factor(x)
                if (na.rm) {
                  u[is.na(x)] <- NA
                }
                for (i in 1:n) {
                  u[i] <- xl[i] %in% g
                }
            }
            return(u)
        }
    for (i in 1:dim(data1)[2]) {
        dum = dummy(data1[, i])
        colnames.dum = paste0(colnames(data1)[i], ".", colnames(dum))
        dum = data.frame(dum[, -dim(dummy(data1[, i]))[2]])
        colnames(dum) = colnames.dum[-dim(dummy(data1[, i]))[2]]
        data0 = data.frame(data0, dum)
    }
    data = data0
    if (out.omit & sum(abs(scale(data)) > 3) > 0) {
        while (sum(abs(scale(data)) > 3) > 0) data[abs(scale(data)) > 
            3, ] = NA
        message("Ouliers set to NA")
    }
    if (na.omit & sum(complete.cases(data)) != dim(data)[1]) {
        data = data[complete.cases(data), ]
        message("Rows with NA removed")
    }
    return(data)
}
