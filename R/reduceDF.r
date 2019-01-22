#' Function reduceDF
#' 
#' Applies LASSO regression and returns a data.frame with the most important predictors.
#' @param data data.frame containing data.
#' @param output index or name of criterion in data.
#' @param y numeric vector.
#' @keywords preprocessing
#' @export
#' @examples
#' data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100));
#' reduceDF(data,"y1")

reduceDF <- function (data, output = NULL, cutoff = 0.1, control = caret::trainControl(method = "boot", 
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
    data2 = data
    cn2 = c(colnames(data), paste0(colnames(data[colnames(data) != 
        "output"]), "Power2"))
    for (i in 1:dim(data[colnames(data) != "output"])[2]) data2 = data.frame(data2, 
        scale(data[colnames(data) != "output"][, i] * data[colnames(data) != 
            "output"][, i]))
    colnames(data2) = cn2
    lambda_max <- max(abs(colSums(as.matrix(scale(data2[, colnames(data2) != 
        "output"])) * as.vector(scale(data2[, "output"])))))/dim(data2)[1]
    epsilon <- 1e-04
    K <- 100
    lambdapath <- round(exp(seq(log(lambda_max), log(lambda_max * 
        epsilon), length.out = K)), digits = 10)
    lambdapath
    glmnet.lambdas = lambdapath
    grid.glmnet0 = expand.grid(lambda = glmnet.lambdas, alpha = 1)
    fit.glmnet0 = caret::train(output ~ ., data = data2, method = "glmnet", 
        preProc = c("center", "scale"), trControl = control, 
        tuneGrid = grid.glmnet0)
    glmnet0 = glmnet(x = as.matrix(data2[names(data2) != "output"]), 
        y = data2[, "output"], lambda = glmnet.lambdas, alpha = fit.glmnet0$bestTune[, 
            "alpha"])
    optimal.coef0 = coef(glmnet0)[, glmnet0$lambda == fit.glmnet0$bestTune[, 
        "lambda"]]
    optimal.coef0
    optimal.coef1 = abs(optimal.coef0[2:dim(data)[2]]) + abs(optimal.coef0[(dim(data)[2] + 
        1):dim(data2)[2]])
    optimal.coef2 = abs(optimal.coef0[2:dim(data)[2]])
    dev.new(width = 14, height = 7)
    par(mfrow = c(1, 2))
    colors = rainbow(dim(data)[2] - 1)[order(optimal.coef1, decreasing = T)]
    colors2 = apply(col2rgb(colors), 2, function(x) rgb(x[1]/255, 
        x[2]/255, x[3]/255, 0.3))
    optimal.coef2 = optimal.coef2[order(optimal.coef1, decreasing = T)]
    optimal.coef1 = optimal.coef1[order(optimal.coef1, decreasing = T)]
    bp(optimal.coef1, col = colors2, main = "Linear and quadratic effects\nbased on LASSO-regression", 
        add.numbers = T)
    title(sub = "Note: linear and quadratic effects separated by a horizontal line", 
        cex.sub = 0.7)
    bp(optimal.coef2, col = colors, add = T)
    colors = rainbow(dim(data)[2] - 1)
    colors2 = apply(col2rgb(colors), 2, function(x) rgb(x[1]/255, 
        x[2]/255, x[3]/255, 0.3))
    colors3 = c(colors, colors2)
    plot(fit.glmnet0$finalModel, "lambda", label = TRUE, xlim = c(min(log(glmnet.lambdas)), 
        0 + abs(min(log(glmnet.lambdas)))/2), ylim = c(-1.5, 
        1.5), lwd = 2, col = colors3)
    abline(h = 0)
    abline(v = log(fit.glmnet0$finalModel$lambdaOpt), lty = 2)
    rangeX = abs(abs(min(log(glmnet.lambdas)))/2 - min(log(glmnet.lambdas)))
    rangeY = c(-1.5, 1.5)
    xl = log(fit.glmnet0$finalModel$lambdaOpt)
    boxedText(xl + rangeX/4, -1.25, xl, -1, paste0("log(Lambda):\n", 
        round(xl, 3)), hspace = 1.2, vspace = 1.5)
    legend("topleft", paste0("Lambda = ", round(fit.glmnet0$finalModel$lambdaOpt, 
        3), "; Alpha =", fit.glmnet0$bestTune[, "alpha"]), box.col = NA, 
        bg = "white")
    box()
    legend("topright", paste(1:length(names(data2)[-1]), ". ", 
        names(data2)[-1]), fill = colors3, cex = 0.7)
    if (sum(optimal.coef1 <= cutoff) > 0) 
        message(paste("skipped:\n", paste(names(optimal.coef1[optimal.coef1 <= 
            cutoff]), collapse = ",")))
    data = data.frame(data[, "output"], data[, names(optimal.coef1[optimal.coef1 > 
        cutoff])])
    names(data)[1] = "output"
    return(data)
}
