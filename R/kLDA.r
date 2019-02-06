#' Function kLDA
#' @keywords modeling
#' @export


kLDA <- function (data = NULL, K = c(2:10), dtm = NULL, method = "Gibbs", 
    removestopwords = F, lower.thresh = 0, seed1 = 0, control = list(nstart = 1, 
        best = T, burnin = 4000, iter = 500, thin = 500, seed = NULL), 
    plot = T, ...) 
{
    if (!is.null(list(...)[["k"]])) 
        stop("Please specify K instad of k!")
    if (is.null(dtm) & is.null(data)) 
        stop("Please provide either a character vector data or Document-Term-Matrix d")
    if (is.null(dtm)) {
        dtm = t(vecToTDM(as.data.frame(data)[[1]], min = 0, plot = F))
        dtm = as.matrix(dtm[!is.na(rownames(dtm)) & !rowSums(dtm) == 
            0, ])
    }
    if (dim(dtm)[1] < 2) 
        stop("DTM contains less than 2 documents")
    if (is.null(control$seed)) 
        control$seed = as.list(1:control$nstart + ifelse(is.null(seed1), 
            0, seed1), control$seed)
    normalize <- function(x) return((x - min(x))/(max(x) - min(x)))
    Arun = function(model, dtm) {
        len <- slam::row_sums(dtm)
        m1 <- exp(model@beta)
        m1.svd <- svd(m1)
        cm1 <- as.matrix(m1.svd$d)
        m2 <- model@gamma
        cm2 <- len %*% m2
        norm <- norm(as.matrix(len), type = "m")
        cm2 <- as.vector(cm2/norm)
        divergence <- sum(cm1 * log(cm1/cm2)) + sum(cm2 * log(cm2/cm1))
        return(divergence)
    }
    Deveaud = function(model) {
        m1 <- exp(model@beta)
        if (any(m1 == 0)) {
            m1 <- m1 + .Machine$double.xmin
        }
        pairs <- utils::combn(nrow(m1), 2)
        jsd <- apply(pairs, 2, function(pair) {
            x <- m1[pair[1], ]
            y <- m1[pair[2], ]
            jsd <- 0.5 * sum(x * log(x/y)) + 0.5 * sum(y * log(y/x))
            return(jsd)
        })
        metric <- sum(jsd)/(model@k * (model@k - 1))
        return(metric)
    }
    ex = numeric(0)
    co = numeric(0)
    ar = numeric(0)
    de = numeric(0)
    for (k in K) {
        l = topicmodels::LDA(dtm, k, method = method, control = control)
        co = c(co, mean(coherenceLDA(l, dtm)))
        ex = c(ex, mean(exclusivityLDA(l, dtm)))
        ar = c(ar, Arun(l, dtm))
        de = c(de, Deveaud(l))
        message(paste0("k=", k, " examined"))
    }
    ar = -1 * ar
    quality = normalize(ex) * normalize(co) * normalize(ar) * 
        normalize(de)
    d1 = data.frame(normalize(ex), normalize(co), normalize(ar), 
        normalize(de), quality)
    d2 = t(scale(data.frame(ex, co, ar, de, quality)))
    d2[is.na(d2)] = -4
    colnames(d2) = K
    rownames(d2) = c("exclusivity", "sem.coherence", "neg. Arun-divergence", 
        "Deveaud-divergence", "overall product")
    if (plot) 
        plotMAT(d2, cumsum = F, xlab = "K", ylab = "value (scaled)", 
            main = "LDA Model quality based on different topics")
    return(list(k = K[which.max(quality)], Results = d2))
}
