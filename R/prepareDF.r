#' Function prepareDF
#' 
#' Scales numeric data and dummy codes factors, omits outliers and missing values.
#' @param data data.frame containing data.
#' @param output index or name of criterion in data.
#' @param out.omit logical value, specifying whether outlier (scale(data)>3) shoud be set to NA.
#' @param na.omit logical value, specifying whether NA shoud be omitted.
#' @keywords preprocessing
#' @export
#' @examples
#' data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100));
#' prepareDF(data,"y1")

prepareDF <- function (data, output = NULL, out.omit = T, na.omit = T) 
{
    require(psych)
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
    data = data.frame(data)
    data0 = data.frame(scale(data[!sapply(data, function(x) is.factor(x))]))
    data1 = data[sapply(data, function(x) is.factor(x))]
    if (dim(data1)[2]) 
        for (i in 1:dim(data1)[2]) {
            dum = dummy.code(data1[, i])
            colnames.dum = paste0(colnames(data1)[i], ".", colnames(dum))
            dum = data.frame(dum[, -dim(dummy.code(data1[, i]))[2]])
            colnames(dum) = colnames.dum[-dim(dummy.code(data1[, 
                i]))[2]]
            data0 = data.frame(data0, dum)
        }
    if (out.omit) 
        while (sum(abs(scale(data)) > 3) > 0) data[abs(scale(data)) > 
            3, ] = NA
    if (na.omit) 
        data = data[complete.cases(data), ]
    return(data)
}
