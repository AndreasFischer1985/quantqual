#' Function predintNNET
#' 
#' Returns and plots prediction interval for a neural network.
#' @param nnet A nnet object.
#' @param xTrain data.frame with one or more numeric vectors.
#' @param yTrain numeric vector.
#' @param xTest data.frame with one or more numeric vectors.  If NULL (default) equals xTrain.
#' @param yTest numeric vector. If NULL (default) equals xTrain.
#' @param color1 Color of points in the scattergram. Defaults to rgb(0,0,0,.7).
#' @param color2 Color of the regression line. Defaults to rgb(0,0,1).
#' @param color3 Color of the prediction interval. Defaults to rgb(0,0,1,.2).
#' @param alpha Alpha level. Defaults to 0.05.
#' @param lambda lambda. Defaults to 0.5.
#' @param funName Activation function for the hidden-layer of the nnet-object ("sigmoid","tanh","linear"). Defaults to "sigmoid".
#' @param fun2Name Activation function for the output-layer of the nnet-object ("sigmoid","tanh","linear"). Defaults to "sigmoid".
#' @param color1 Color of points in the scattergram. Defaults to rgb(0,0,0,.7).
#' @param color2 Color of the regression line. Defaults to rgb(0,0,1).
#' @param color3 Color of the prediction interval. Defaults to rgb(0,0,1,.2).
#' @details Returns and plots prediction interval for a neural network. This function is a modification of nnetpredint::nnetPredInt version 1.2. The code was modified by Andreas Fischer to allow for prediction intervals of nnet-objetcs with linear output units (i.e., parameter linout=T in nnet::nnet). The nnetpredint-package (https://cran.r-project.org/package=nnetpredint) was written by Xichen Ding <rockingdingo at gmail.com> under GPL-License (>= 2). There is no waranty for the work whatsoever.
#' @keywords modeling
#' @export
#' @examples
#' set.seed(0);
#' d=data.frame(x=scale(rnorm(100)*10+1:100),y=scale(rnorm(100)*10+1:100),z=scale(rnorm(100)*10+1:100));
#' d=d[order(d[,"x"]),]
#' dev.new();
#' n=nnet::nnet(y ~ ., data=d, size=2, rang=0.1, decay=5e-4, maxit=500, linout=T)
#' p=predintNNET(nnet=n, xTrain=d[c("x","z")], yTrain=d["y"], alpha = 0.05, funName = "sigmoid", fun2Name = "linear")
#' dev.new();
#' n=nnet::nnet(y ~ x, data=d, size=2, rang=0.1, decay=5e-4, maxit=500, linout=T)
#' p=predintNNET(nnet=n, xTrain=d[c("x")], yTrain=d["y"], alpha = 0.05, funName = "sigmoid", fun2Name = "linear")


predintNNET <- function (nnet = NULL, xTrain = NULL, yTrain = NULL, xTest = NULL, 
    yTest = NULL, alpha = 0.05, lambda = 0.5, funName = "sigmoid", 
    fun2Name = "linear", main = "Nonlinear Regression", sub = "shaded area represent prediction interval.", 
    xlab = "Predictor", ylab = "Criterion", plot = T, col1 = rgb(0, 
        0, 0, 0.8), col2 = rgb(0, 0, 1), col3 = rgb(0, 0, 1, 
        0.2), pch = 16, lwd = 2, cex.sub = 0.7, ...) 
{
    nnetPredInt <- function(object, xTrain, yTrain, newData, 
        alpha = 0.05, lambda = 0.5, funName = "sigmoid", fun2Name = "linear", 
        ...) {
        wts = object$wts
        nodeNum = object$n
        yFit = c(object$fitted.values)
        yPredInt = getPredInt(xTrain, yTrain, yFit, nodeNum, 
            wts, newData, alpha = alpha, lambda = lambda, funName = funName, 
            fun2Name = fun2Name)
        return(yPredInt)
    }
    sigmoid <- function(x) {
        y = 1/(1 + exp(-x))
        return(y)
    }
    sigmoidDeri <- function(x) {
        y = sigmoid(x) * (1 - sigmoid(x))
        return(y)
    }
    tanhFunc <- function(x) {
        y = tanh(x)
        return(y)
    }
    tanhDeri <- function(x) {
        y = 1 - (tanh(x))^2
        return(y)
    }
    activate <- function(x, funName) {
        if (funName == "linear") {
            res = x
        }
        else if (funName == "sigmoid") {
            res = sigmoid(x)
        }
        else if (funName == "tanh") {
            res = tanhFunc(x)
        }
        else {
            res = "invalid activation function"
        }
        return(res)
    }
    activateDeri <- function(x, funName) {
        if (funName == "linear") {
            res = 1
        }
        else if (funName == "sigmoid") {
            res = sigmoidDeri(x)
        }
        else if (funName == "tanh") {
            res = tanhDeri(x)
        }
        else {
            res = "invalid activation function"
        }
        return(res)
    }
    getOutVectList <- function(xVect, W, B, m, funName, fun2Name) {
        outVecList = list()
        aVectList = list()
        curAVect = xVect
        for (k in 1:m) {
            wk = W[[k]]
            bVect = B[[k]]
            curOutVect = wk %*% matrix(curAVect) + matrix(bVect)
            curAVect = ifelse(k != length(W), list(activate(curOutVect, 
                funName)), list(activate(curOutVect, fun2Name)))[[1]]
            outVecList = c(outVecList, list(curOutVect))
            aVectList = c(aVectList, list(curAVect))
        }
        res = list(out = outVecList, aVect = aVectList)
        return(res)
    }
    getPredVal <- function(xVect, W, B, m, funName, fun2Name) {
        yPred = 0
        curAVect = xVect
        for (k in 1:m) {
            wk = W[[k]]
            bVect = B[[k]]
            curOutVect = wk %*% matrix(curAVect) + matrix(bVect)
            curAVect = ifelse(k != length(W), list(activate(curOutVect, 
                funName)), list(activate(curOutVect, fun2Name)))[[1]]
        }
        yPred = c(curAVect)
        return(yPred)
    }
    transWeightPara <- function(Wts, m, nodeNum) {
        idxPara = 0
        W = list()
        B = list()
        for (k in 1:m) {
            Sk = nodeNum[k + 1]
            Sk_1 = nodeNum[k]
            curWts = Wts[(idxPara + 1):(idxPara + Sk * (Sk_1 + 
                1))]
            curWtsMat = t(matrix(curWts, nrow = (Sk_1 + 1)))
            W = c(W, list(matrix(curWtsMat[, 2:(Sk_1 + 1)], nrow = Sk)))
            B = c(B, list(matrix(curWtsMat[, 1:1], nrow = Sk)))
            idxPara = idxPara + Sk * (Sk_1 + 1)
        }
        res = list(W = W, B = B)
        return(res)
    }
    calcSigmaEst <- function(yTarVal, yPredVal, nObs, nPara) {
        if (length(c(yTarVal)) != length(c(yPredVal))) {
            return(0)
        }
        else {
            nDegreeFree = nObs - nPara
            varEst = sum((c(yTarVal) - c(yPredVal))^2)/nDegreeFree
            sigma = sqrt(varEst)
        }
        return(sigma)
    }
    parOutVectGradMat <- function(idx, k, m, outVect, W, funName, 
        fun2Name) {
        if (idx == k) {
            nDim = dim(outVect[[idx]])[1]
            diagMat = matrix(rep(0, nDim * nDim), nrow = nDim)
            diag(diagMat) = c(activateDeri(outVect[[idx]], fun2Name))
            res = diagMat
        }
        else if (idx > k) {
            nDim = dim(outVect[[idx]])[1]
            diagMat = matrix(rep(0, nDim * nDim), nrow = nDim)
            diag(diagMat) = c(activateDeri(outVect[[idx]], funName))
            res = diagMat %*% W[[idx]] %*% parOutVectGradMat(idx - 
                1, k, m, outVect, W, funName, fun2Name)
        }
        return(res)
    }
    getParaGrad <- function(xVect, W, B, m, nPara, funName, fun2Name) {
        outVect = getOutVectList(xVect, W, B, m, funName, fun2Name)$out
        aVect = getOutVectList(xVect, W, B, m, funName, fun2Name)$aVect
        gradParaVect = c()
        for (k in 1:m) {
            wk = W[[k]]
            wkDeriVect = c()
            curGradActiMat = parOutVectGradMat(idx = m, k, m, 
                outVect, W, funName, fun2Name)
            if (k == 1) {
                multiplier = matrix(xVect)
            }
            else {
                multiplier = matrix(activateDeri(outVect[[k - 
                  1]], funName))
            }
            numSk = dim(wk)[1]
            numSk_1 = dim(wk)[2]
            for (i in 1:numSk) {
                wkiVect = rep(0, (numSk_1 + 1))
                bVectDeri = 0 * B[[k]]
                bVectDeri[i] = 1
                bikDeriVal = curGradActiMat %*% matrix(bVectDeri)
                wkiVect[1] = bikDeriVal
                for (j in 1:numSk_1) {
                  wkDeri = 0 * wk
                  wkDeri[i, j] = 1
                  wijkDeriVal = curGradActiMat %*% wkDeri %*% 
                    multiplier
                  wkiVect[j + 1] = wijkDeriVal
                }
                wkDeriVect = c(wkDeriVect, wkiVect)
            }
            gradParaVect = c(gradParaVect, wkDeriVect)
        }
        return(gradParaVect)
    }
    parGetParaGrad <- function(idx, xMat, W, B, m, nPara, funName, 
        fun2Name) {
        curWtsDeri = getParaGrad(matrix(as.numeric(xMat[idx, 
            ])), W, B, m, nPara, funName, fun2Name)
        return(curWtsDeri)
    }
    getJacobianMatrix <- function(xTrain, W, B, m, nPara, funName, 
        fun2Name) {
        nObs = dim(xTrain)[1]
        jacobianMat = matrix(rep(0, nObs * nPara), nrow = nObs)
        idx = c(1:nObs)
        res = sapply(idx, FUN = parGetParaGrad, xMat = xTrain, 
            W = W, B = B, m = m, nPara = nPara, funName = funName, 
            fun2Name = fun2Name)
        jacobianMat = t(res)
        return(jacobianMat)
    }
    jacobian <- function(object, xTrain, funName = "sigmoid", 
        fun2Name = "linear", ...) {
        wts = object$wts
        nodeNum = object$n
        m = length(nodeNum) - 1
        nPara = length(wts)
        transWts = transWeightPara(wts, m, nodeNum)
        W = transWts$W
        B = transWts$B
        jacobianMat = getJacobianMatrix(xTrain, W, B, m, nPara, 
            funName, fun2Name)
        return(jacobianMat)
    }
    getPredInt <- function(xTrain, yTrain, yFit, node, wts, newData, 
        alpha = 0.05, lambda = 0.5, funName = "sigmoid", fun2Name = "linear") {
        nObs = dim(xTrain)[1]
        nPara = length(wts)
        nPred = dim(newData)[1]
        m = length(node) - 1
        transWts = transWeightPara(wts, m, node)
        W = transWts$W
        B = transWts$B
        predIntMat = matrix(rep(0, 2 * nPred), ncol = 2)
        yPredVect = c()
        sigmaGaussion = calcSigmaEst(yTrain, yFit, nObs, nPara)
        tQuant = qt(alpha/2, df = (nObs - nPara))
        jacobianMat = getJacobianMatrix(xTrain, W, B, m, nPara, 
            funName, fun2Name)
        errorSingular = try(solve(t(jacobianMat) %*% jacobianMat), 
            silent = TRUE)
        if (class(errorSingular) == "try-error") {
            jacobianInvError = solve(t(jacobianMat) %*% jacobianMat + 
                lambda * diag(nPara)) %*% t(jacobianMat) %*% 
                jacobianMat %*% solve(t(jacobianMat) %*% jacobianMat + 
                lambda * diag(nPara))
        }
        else {
            jacobianInvError = solve(t(jacobianMat) %*% jacobianMat)
        }
        for (i in 1:nPred) {
            xVect = matrix(as.numeric(newData[i:i, ]))
            yPred = getPredVal(xVect, W, B, m, funName, fun2Name)
            wtsGradVect = getParaGrad(xVect, W, B, m, nPara, 
                funName, fun2Name)
            f = matrix(wtsGradVect)
            sigmaModel = sqrt(1 + t(f) %*% jacobianInvError %*% 
                f)
            confWidth = abs(tQuant * sigmaGaussion * sigmaModel)
            predIntVect = c(yPred - confWidth, yPred + confWidth)
            predIntMat[i:i, ] = predIntVect
            yPredVect = c(yPredVect, yPred)
        }
        resDf = data.frame(yPredValue = yPredVect, lowerBound = predIntMat[, 
            1:1], upperBound = predIntMat[, 2:2])
        return(resDf)
    }
    if (is.null(nnet) & is.null(xTrain) & is.null(yTrain)) {
        d = data.frame(x = scale(rnorm(100) * 10 + 1:100), y = scale(rnorm(100) * 
            10 + 1:100), z = scale(rnorm(100) * 10 + 1:100))
        d = d[order(d[, "x"]), ]
        nnet = nnet::nnet(y ~ x, data = d, size = 2, rang = 0.1, 
            decay = 5e-04, maxit = 500, linout = T)
        xTrain = d[c("x")]
        yTrain = d["y"]
    }
    if (is.null(nnet) | is.null(xTrain) | is.null(yTrain)) 
        stop("Please provide a nnet object as well as input (xTrain) and output (yTrain).")
    if (sum(grepl("skip", deparse(nnet$call))) == T) 
        warning("parameter 'skip' will be ignored")
    xTrain = as.matrix(xTrain)
    yTrain = as.matrix(yTrain)
    if (is.null(xTest)) 
        xTest = xTrain
    xTest = as.matrix(xTest)
    if (is.null(yTest)) 
        yTest = yTrain
    yTest = as.matrix(yTest)
    predint = nnetPredInt(object = nnet, xTrain = xTrain, yTrain = yTrain, 
        newData = xTest, alpha = alpha, lambda = lambda, funName = funName, 
        fun2Name = fun2Name)
    if (plot) {
        xTest0 = xTest
        if (dim(xTest)[2] > 1) {
            warning("More than one predictor.")
            xTest0 = apply(xTest, 2, as.numeric) %*% rep(1/dim(xTest)[2], 
                dim(xTest)[2])
        }
        dat = data.frame(xTest0, yTest, predint)
        dat = dat[order(xTest0), ]
        plot(dat[, 1], dat[, 2], type = "n", main = main, xlab = xlab, 
            ylab = ylab, ...)
        polygon(c(dat[, 1], dat[dim(dat)[1]:1, 1]), c(dat[, 4], 
            dat[dim(dat)[1]:1, 5]), col = col3, border = NA)
        points(dat[, 1], dat[, 2], pch = pch, col = col1)
        lines(dat[, 1], dat[, 3], col = col2, lwd = lwd)
        if (!is.null(sub)) 
            title(sub = sub, cex.sub = cex.sub)
    }
    return(predint)
}
