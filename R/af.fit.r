#' Function af.fit
#' 
#' Fits nnet, glmnet and linear regression (with and without interaction terms) to a data.frame.
#' @param data.train data.frame containing training data.
#' @param output index or name of criterion in training data.
#' @param rep.nnet number of random initialisations that should be applied for choosing af.nnet.
#' @keywords modeling
#' @export
#' @examples
#' data=data.frame(y=rnorm(100),x1=rnorm(100),x2=rnorm(100));
#' fit.quant(data,"y1")

af.fit <- function (data.train, output = NULL, rep.nnet = 5, preProc = c("center", 
    "scale"), control = caret::trainControl(method = "boot", 
    number = 10), nnet.linout = T, nnet.maxit = 1000, nnet.trace = F, 
    ...) 
{
    require(nnet)
    if (sum(grepl("center", preProc)) > 0) 
        data.train = apply(data.train, 2, function(x) x - mean(x, 
            na.rm = T))
    if (sum(grepl("scale", preProc)) > 0) 
        data.train = apply(data.train, 2, function(x) x/sd(x, 
            na.rm = T))
    data.train = data.frame(data.train)
    if (is.null(names(data.train))) 
        names(data.train) = 1:dim(data.train)[2]
    if (is.numeric(output)) 
        output = ifelse(output > dim(data.train)[2], 1, output)
    if (is.character(output)) 
        output = ifelse(length(grep(output, names(data.train))) == 
            0, 1, which(names(data.train) == output)[1])
    if (!is.null(output)) 
        names(data.train)[output] = "output"
    if (is.null(output)) 
        names(data.train)[1] = "output"
    af.nnet = list(label = "Andreas Fischer's Neural Network", 
        library = "nnet", loop = NULL, type = c("Classification", 
            "Regression"), parameters = data.frame(parameter = c("size", 
            "decay"), class = rep("numeric", 2), label = c("Units", 
            "Decay")), grid = function(x, y, len = NULL, search = "grid") {
            if (search == "grid") {
                out <- expand.grid(size = ((1:len) * 2) - 1, 
                  decay = c(0, 10^seq(-1, -4, length = len - 
                    1)))
            } else {
                out <- data.frame(size = sample(1:20, size = len, 
                  replace = TRUE), decay = 10^runif(len, min = -5, 
                  1))
            }
            out
        }, fit = function(x, y, wts, param, lev, last, classProbs, 
            repetitions = rep.nnet, ...) {
            dat <- if (is.data.frame(x)) {
                x
            } else {
                as.data.frame(x)
            }
            dat$.outcome <- y
            if (!is.null(wts)) {
                out <- lapply(c(1:repetitions), function(x) nnet::nnet(.outcome ~ 
                  ., data = dat, weights = wts, size = param$size, 
                  decay = param$decay, ...))
                out <- out[order(sapply(out, function(x) cor(predict(x), 
                  dat$.outcome, use = "pairwise")), decreasing = T)][[1]]
            } else {
                out <- lapply(c(1:repetitions), function(x) nnet::nnet(.outcome ~ 
                  ., data = dat, size = param$size, decay = param$decay, 
                  ...))
                out <- out[order(sapply(out, function(x) cor(predict(x), 
                  dat$.outcome, use = "pairwise")), decreasing = T)][[1]]
            }
            out
        }, predict = function(modelFit, newdata, submodels = NULL) {
            if (modelFit$problemType == "Classification") {
                out <- predict(modelFit, newdata, type = "class")
            } else {
                out <- predict(modelFit, newdata, type = "raw")
            }
            out
        }, prob = function(modelFit, newdata) predict(modelFit, 
            newdata), varImp = function(object, ...) {
            imp <- caret:::GarsonWeights(object, ...)
            if (ncol(imp) > 1) {
                imp <- cbind(apply(imp, 1, mean), imp)
                colnames(imp)[1] <- "Overall"
            } else {
                imp <- as.data.frame(imp)
                names(imp) <- "Overall"
            }
            if (!is.null(object$xNames)) rownames(imp) <- object$xNames
            imp
        }, tags = c("Neural Network", "L2 Regularization", "Accepts Case Weights", 
            "Multiple Initialisations"), levels = function(x) lev(x), 
        sort = function(x) x[order(x$size, -x$decay), ])
    grid.nnet = expand.grid(size = 1:6, decay = c(0, 10^seq(-5, 
        1, by = 1)))
    fits = list()
    message(paste0("Started fiting at ", Sys.time()))
    fits[["lm"]] = caret::train(output ~ ., data = data.train, 
        method = "lm", preProc = preProc, trControl = control)
    message(names(fits)[length(fits)])
    message(Sys.time())
    fits[["lm.Int"]] = caret::train(output ~ . * ., data = data.train, 
        method = "lm", preProc = preProc, trControl = control)
    message(names(fits)[length(fits)])
    message(Sys.time())
    fits[["glmnet"]] = caret::train(output ~ ., data = data.train, 
        method = "glmnet", preProc = preProc, trControl = control)
    message(names(fits)[length(fits)])
    message(Sys.time())
    fits[["nnet"]] = caret::train(output ~ ., data = data.train, 
        method = "nnet", preProc = preProc, trControl = control, 
        tuneGrid = grid.nnet, linout = nnet.linout, maxit = nnet.maxit, 
        trace = nnet.trace, ...)
    message(names(fits)[length(fits)])
    message(Sys.time())
    fits[["af.nnet"]] = caret::train(output ~ ., data = data.train, 
        method = af.nnet, preProc = preProc, trControl = control, 
        tuneGrid = grid.nnet, linout = nnet.linout, maxit = nnet.maxit, 
        trace = nnet.trace)
    message(names(fits)[length(fits)])
    message(Sys.time())
    fits = fits[order(cor(data.frame(data.train[, "output"], 
        lapply(fits, function(x) predict(x, data.train))))[-1, 
        1], decreasing = T)]
    quantqual::bp(cor(data.frame(data.train[, "output"], lapply(fits, 
        function(x) predict(x, data.train))))[-1, 1], ylim = c(0, 
        1), add.numbers = T, main = "Model Comparison")
    return(fits)
}
