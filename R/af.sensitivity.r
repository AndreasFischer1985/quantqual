#' Function af.sensitivity
#' 
#' Plots Lek-Pofiles and Fischer's Deltas.
#' @param model Any object that works as input for function predict.
#' @param x data.frame with numeric predictor variables.
#' @param y numeric criterion variable.
#' @details Plots Lek-Pofiles and Fischer's Deltas (i.e., the difference in R^2 that results from omitting a predictor for each predictor, as described by Fischer(2015)).
#' @keywords modeling
#' @export
#' @examples
#' data=data.frame(y=rnorm(100)+scale(1:100),x1=rnorm(100)+scale(1:100),x2=rnorm(100));
#' l=lm(y~.,data=data)
#' sensitivity(l,data[c("x1","x2")],data["y"])

af.sensitivity <- function (model, x = NULL, y = NULL, steps = 100, splitseq = seq(0, 
    1, by = 0.25), plot = T, ...) 
{
    el = list(...)
    if (is.null(x) | is.null(y)) 
        stop("Please specify predictor x and criterion y!")
    x = as.data.frame(x)
    y = as.data.frame(y)
    if (is.null(colnames(x))) 
        colnames(x) = paste0("Input ", 1:dim(x)[2])
    con = deltas(model, x, y, plot = F)
    vars = dim(x)[2]
    splits = length(splitseq)
    ar = array(dim = c(steps, dim(x)[2]))
    for (i in 1:vars) ar[, i] = seq(from = min(x[, i]), to = max(x[, 
        i]), length.out = steps)
    ar1 = array(dim = c(steps, vars, splits))
    ar2 = array(dim = c(steps, splits, vars))
    for (j in 1:splits) for (i in 1:vars) {
        for (k in 1:steps) {
            for (z in 1:vars) ar1[k, z, j] = quantile(x[, z], 
                probs = splitseq)[j]
            ar1[k, i, j] = ar[k, i]
        }
        dat = data.frame(ar1[, , j])
        colnames(dat) = colnames(x)
        ar2[, j, i] = predict(model, newdata = dat)
    }
    ar = ar2
    tryCatch({
        ar = (ar2 - mean(y[[1]], na.rm = T))/sd(y[[1]], na.rm = T)
    }, error = function(cond) {
    })
    re = array(dim = c(steps, vars))
    for (j in 1:vars) for (k in 1:steps) re[k, j] = median(ar[k, 
        , j])
    if (plot == T) {
        plot(seq(1:steps), re[, 1], xlim = c(0, steps), ylim = ifelse(length(el[["ylim"]]) > 
            0, el["ylim"], list(c(min(y), max(y))))[[1]], main = ifelse(length(el[["main"]]) > 
            0, el["main"], list(paste0("Sensitivity Analysis\n", 
            "(", expression(R^2), "=", round(con[length(con)], 
                2), ")")))[[1]], type = "n", ylab = "median prediction", 
            xlab = "% of input range")
        colors1 = apply(col2rgb(rainbow(vars)), 2, function(x) rgb(x[1]/255, 
            x[2]/255, x[3]/255, 1))
        colors2 = apply(col2rgb(rainbow(vars)), 2, function(x) rgb(x[1]/255, 
            x[2]/255, x[3]/255, 0.3))
        for (i in 1:vars) {
            for (s in 1:splits) lines(seq(1:steps), ar[, s, i], 
                lty = i, lwd = 1, col = colors2[i])
            lines(seq(1:steps), re[, i], lty = i, lwd = 3, col = colors1[i])
        }
        legend("bottomright", legend = paste0(colnames(x), " (", 
            expression(R^2), "=", round(con[1:length(con) - 1], 
                2), ")"), lty = 1:vars, col = colors1, bg = "white", 
            inset = 0.01, cex = 0.7)
    }
    ri = array(dim = c(vars))
    for (i in 1:vars) ri[i] = max(re[, i]) - min(re[, i])
    return(data.frame(Fischer.Delta = con[-length(con)], Lek.Range = ri, 
        row.names = colnames(x)))
}
