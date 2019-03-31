#' Function af.lasso
#' 
#' Applies LASSO regression and returns data.frame with important predictors.
#' @param data data.frame containing data.
#' @param output index or name of criterion in data.
#' @param y numeric vector.
#' @keywords preprocessing
#' @export
#' @examples
#' data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100));
#' af.lasso(data,"y1")

af.lasso <- function (data, output = NULL, control = caret::trainControl(method = "boot", 
    number = 30)) 
{
    require(glmnet)
    require(caret)
    data = data.frame(scale(data))
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
    lambda_max <- max(abs(colSums(as.matrix(scale(data[, colnames(data) != 
        "output"])) * as.vector(scale(data[, "output"])))))/dim(data)[1]
    epsilon <- 1e-04
    K <- 100
    lambdapath <- round(exp(seq(log(lambda_max), log(lambda_max * 
        epsilon), length.out = K)), digits = 10)
    lambdapath
    glmnet.lambdas = lambdapath
    grid.glmnet0 = expand.grid(lambda = glmnet.lambdas, alpha = 1)
    fit.glmnet0 = caret::train(output ~ ., data = data, method = "glmnet", 
        preProc = c("center", "scale"), trControl = control, 
        tuneGrid = grid.glmnet0)
    glmnet0 = glmnet(x = as.matrix(data[names(data) != "output"]), 
        y = data[, "output"], lambda = glmnet.lambdas, alpha = fit.glmnet0$bestTune[, 
            "alpha"])
    optimal.coef0 = coef(glmnet0)[, glmnet0$lambda == fit.glmnet0$bestTune[, 
        "lambda"]]
    optimal.coef0 = optimal.coef0[names(optimal.coef0) != "(Intercept)"]
    par(mfrow = c(1, 2))
    colors = rainbow(dim(data)[2] - 1)[order(optimal.coef0, decreasing = T)]
    optimal.coef0 = optimal.coef0[order(optimal.coef0, decreasing = T)]
    quantqual::bp(optimal.coef0, col = colors, main = "Linear effects\nbased on LASSO-regression", 
        add.numbers = T)
    colors = rainbow(dim(data)[2] - 1)
    plot(fit.glmnet0$finalModel, "lambda", label = TRUE, xlim = c(min(log(glmnet.lambdas)), 
        0 + abs(min(log(glmnet.lambdas)))/2), ylim = c(-1.5, 
        1.5), lwd = 2, col = colors)
    abline(h = 0)
    abline(v = log(fit.glmnet0$finalModel$lambdaOpt), lty = 2)
    rangeX = abs(abs(min(log(glmnet.lambdas)))/2 - min(log(glmnet.lambdas)))
    rangeY = c(-1.5, 1.5)
    xl = log(fit.glmnet0$finalModel$lambdaOpt)
    legend("bottomleft", paste0("log(Lambda) = ", round(xl, 3), 
        "; Alpha =", fit.glmnet0$bestTune[, "alpha"]), box.col = NA, 
        bg = "white")
    box()
    legend("topright", paste(1:length(names(data)[-1]), ". ", 
        names(data)[-1]), fill = colors, cex = 0.7, bg = "white")
    attr(glmnet0, "opt.lambda") = fit.glmnet0$finalModel$lambdaOpt
    attr(glmnet0, "opt.coef") = coef(glmnet0)[, glmnet0$lambda == 
        fit.glmnet0$bestTune[, "lambda"]]
    return(glmnet0)
}
