#' Function af.nnet
#' 
#' Fits multiple initializations of nnet to scaled data and returns best nnet-object.
#' @param data.train data.frame containing training data.
#' @param output index or name of criterion in training data.
#' @param size numeric value determining the number of hidden units.
#' @param decay numeric value determining the decay parameter.
#' @details Fits multiple initializations of nnet to scaled data and returns best nnet-object. If size or decay are NULL, 10-fold cross validation is applied to find an optimal set of parameters
#' @keywords modeling
#' @export
#' @examples
#'  

af.nnet <- function (data.train, output = NULL, rep.nnet = 5, seed = 0, 
    plot = F, size = 3, decay = 0, linout = T, trace = F, ...) 
{
    require(nnet)
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
    set.seed(seed)
    out <- NULL
    control = caret::trainControl(method = "cv", number = 10)
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
    if (is.null(size) & is.null(decay)) {
        grid.nnet = expand.grid(size = 1:6, decay = c(0, 10^seq(-5, 
            1, by = 1)))
        out1 <- caret::train(output ~ ., data = data.train, method = af.nnet, 
            preProc = c("center", "scale"), trControl = control, 
            tuneGrid = grid.nnet, linout = T, maxit = 1000, trace = F)
        out = out1$finalModel
    }
    else if (!is.null(size) & is.null(decay)) {
        grid.nnet = expand.grid(size = size, decay = c(0, 10^seq(-5, 
            1, by = 1)))
        out1 <- caret::train(output ~ ., data = data.train, method = af.nnet, 
            preProc = c("center", "scale"), trControl = control, 
            tuneGrid = grid.nnet, linout = T, maxit = 1000, trace = F)
        out = out1$finalModel
    }
    else if (is.null(size) & !is.null(decay)) {
        grid.nnet = expand.grid(size = 1:6, decay = decay)
        out1 <- caret::train(output ~ ., data = data.train, method = af.nnet, 
            preProc = c("center", "scale"), trControl = control, 
            tuneGrid = grid.nnet, linout = T, maxit = 1000, trace = F)
        out = out1$finalModel
    }
    else {
        out <- lapply(c(1:rep.nnet), function(x) nnet::nnet(output ~ 
            ., data = data.train, size = size, decay = decay, 
            linout = linout, trace = trace))
        out <- out[order(sapply(out, function(x) cor(predict(x), 
            data.train$output, use = "pairwise")), decreasing = T)][[1]]
    }
    if (plot) 
        plot(predict(out), data.train$output, col = "blue", lwd = 3, 
            main = "Performance")
    print(out$tuneValue)
    return(out)
}
