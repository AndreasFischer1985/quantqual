#' Function outlier.anaysis
#' 
#' Determines outliers based on boxplots mahalanobis distance.
#' @param df A data.frame containing numeric vectors.
#' @details Determines outliers based on boxplots and mahalanobis distance. Returns boxplots with additional attribute "mahalanobis"
#' @keywords preprocessing
#' @export
#' @examples
#' outlier.analysis(data.frame(c(1,2,100)))

outlier.analysis <- function (df = NULL, cutoff = NULL, plot = T, col = rgb(106/255, 
    160/255, 186/255), varwidth = T, outline = T, main = "Outlier Analysis", 
    cex = 0.8, ...) 
{
    if (is.null(df)) 
        df = c(Unknown = 4067, `Russian Federation` = 4884, Somalia = 6836, 
            Nigeria = 7811, Turkey = 8027, Iran = 8608, Eritrea = 10226, 
            Afghanistan = 16423, Iraq = 21930, Syria = 48974)
    rn = rownames(df)
    if (is.data.frame(df)) 
        df = scale(df[sapply(df, is.numeric)])
    if (is.numeric(df)) 
        df = scale(df)
    rownames(df) = rn
    if (is.null(rownames(df))) 
        rownames(df) = 1:dim(df)[1]
    b = boxplot(df, varwidth = varwidth, outline = outline, col = col, 
        main = main, plot = plot, ...)
    if (length(b$out) != 0) {
        out.rows <- sapply(1:length(b$out), function(i) which(df[, 
            b$group[i]] == b$out[i]))
        text(b$group, b$out, rownames(df)[out.rows], pos = 4, 
            cex = cex, col = col)
    }
    dist <- mahalanobis(df, colMeans(df, na.rm = T), cov(df, 
        use = "pairwise.complete.obs"))
    if (!is.null(cutoff)) 
        attr(b, "mahalanobis") = (dist > cutoff)
    if (is.null(cutoff)) 
        attr(b, "mahalanobis") = (round(dist, 2))
    return(b)
}
