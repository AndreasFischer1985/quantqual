#' Function deltas
#' 
#' Calculates the difference in \eqn{expression(R^2)} that results from omitting a predictor for each predictor.
#' @param model Any object that works as input for function predict.
#' @param x data.frame with numeric vectors.
#' @param y numeric vector.
#' @param fun A function that specifies what a predictor should be replaced with to remove its influence. Defaults at mean.
#' @param plot Logical value indicating whether to plot the barplot. Defaults zo T.
#' @param main Character vector with one element containing the barplot's title. Defaults to "Fischer's Delta".
#' @param xlab Character vector with one element containing the barplot's x-axis label. Defaults to "Predictor".
#' @param ylab Character vector with one element containing the barplot's y-axis label. Defaults to "\eqn{R^2}".
#' @param col Vector containing the color of bars. If NULL (default) colors are generated based on the rainbow-palette.
#' @param ... Additional graphical parameters for barplot.
#' @details Calculates the difference in R^2 that results from omitting a predictor for each predictor as described by Fischer(2015).
#' @keywords modeling
#' @export
#' @references Fischer (2015). How to determine the unique contributions of input-variables to the nonlinear regression function of a multilayer perceptron. Ecological Modelling, 309, 60-63.
#' @examples
#' data=data.frame(y=rnorm(100)+scale(1:100),x1=rnorm(100)+scale(1:100),x2=rnorm(100));
#' l=lm(y~.,data=data)
#' deltas(l,data[c("x1","x2")],data["y"])

deltas <- function (model, x = NULL, y = NULL, fun = mean, plot = T, main = "Fischer's Delta", 
    xlab = "Predictor", ylab = expression(R^2), col = NULL, ...) 
{
    el = list(...)
    if (is.null(x) | is.null(y)) {
        if (!is.na(match("lm", class(model)))) {
            if (is.null(y)) 
                y = l$model[, 1]
            if (is.null(x)) 
                x = l$model[, -1]
        }
        else if (!is.na(match("nnet", class(model)))) {
            mc = as.list(model$call)
            if (!is.null(mc$x) & !is.null(mc$x)) {
                if (is.null(y)) 
                  y = eval(mc$y)
                if (is.null(x)) 
                  x = eval(mc$x)
            }
            else if (!is.null(mc$formula) & !is.null(mc$data)) {
                if (is.null(y)) 
                  y = eval(mc$data)[, as.character(mc$formula)[2]]
                if (is.null(x)) 
                  x = eval(mc$data)[, names(model$xlevels)]
            }
        }
        if (verbose == T) {
            message("x:")
            print(x)
            message("y:")
            print(y)
        }
    }
    if (is.null(x) | is.null(y)) 
        stop("Please specify predictor x and criterion y!")
    if (sd(predict(model), na.rm = T) == 0) 
        stop("No variation in model predictions")
    if (dim(data.frame(x))[2] < 2) {
        val = rep(cor(predict(model), data.frame(y)[[1]])[1]^2, 
            2)
        names(val) = c("x1", "reference")
        return(val)
    }
    l = list()
    for (j in 1:dim(data.frame(x))[2]) {
        d = data.frame(x)
        for (i in 1:dim(d)[2]) {
            if (i == j) 
                d[, i] = ifelse(!is.factor(d[, i]), list(fun(d[, 
                  i], na.rm = T)), list(names(sort(table(d[, 
                  i]), decreasing = T)[1])))
        }
        l[[j]] = predict(model, newdata = d)
    }
    l[[dim(data.frame(x))[2] + 1]] = predict(model, newdata = data.frame(x))
    l = as.data.frame(l)
    names(l) = c(1:(length(l) - 1))
    names(l)[length(l)] = c("reference")
    val = (cor(data.frame(l, y))[length(l) + 1, ][-(length(l) + 
        1)])^2
    val[1:(length(val) - 1)] = val[length(val)] - (val[1:(length(val) - 
        1)])
    val[apply(l, 2, var) == 0] = val[length(val)]
    if (plot == T) {
        if (is.null(col)) 
            col = rainbow(length(val) - 1)
        bp = barplot(val[1:(length(val) - 1)], ylab = ylab, xlab = xlab, 
            ylim = c(0, 1), col = col, main = main, ...)
        text(x = bp, y = val[1:(length(val) - 1)], labels = round(val[1:(length(val) - 
            1)], 2), pos = 3, xpd = T)
        title(sub = paste("(Overall", expression(R^2), "=", round(val[length(l)], 
            2), ")"))
    }
    return(val)
}
