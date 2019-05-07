#' Function qualityLDA
#' 
#' Calculates and plots accuracyLDA, logLik, AIC and BIC for one or more LDA-objects.
#' @param lda Object of class LDA (or list of LDA objects).
#' @param dtm document-term matrix.
#' @plot Logical value specifying whether to plot results. Defaults to F.
#' @param n Parameter to be passed to accuracyLDA. Number of top-terms to search for in the documents. Defaults to 5.
#' @param type Parameter to be passed to accuracyLDA. Determines how the results are aggregated. Defaults to 1. If type==0 the function returns the number of each document's top-topic's n top-terms that are actually present in the document as a vector e; type==1 returns mean(e>0); type==2 returns mean(e==n); type==3 returns mean(e);  type==4 returns median(e>0); type==5 returns median(e==n); type==6 returns median(e);  type==7 returns min(e>0); type==8 returns min(e==n); type==9 returns min(e); 
#' @keywords modeling
#' @export
#' @examples
#' accuracyLDA(lda,dtm)

qualityLDA <- function (lda, dtm, plot = F, n = 5, type = 1) 
{
    if (!is.list(lda)) 
        lda = list(lda)
    d = sapply(1:length(lda), function(x) {
        l = lda[[x]]
        c(accuracy = quantqual::accuracyLDA(l, dtm, type = type), 
            logLik = logLik(l), AIC = AIC(l), BIC = BIC(l))
    })
    if (plot == T) {
        k = sapply(lda, function(x) attr(x, "k"))
        par(mfrow = c(2, 2))
        plot(k, d[1, ], xlab = "k", ylab = "Accuracy", type = "b", 
            main = "Accuracy", ylim = c(0, 1))
        plot(k, d[2, ], xlab = "k", ylab = "logLik", type = "b", 
            main = "logLik")
        plot(k, d[3, ], xlab = "k", ylab = "AIC", type = "b", 
            main = "AIC")
        plot(k, d[4, ], xlab = "k", ylab = "BIC", type = "b", 
            main = "BIC")
    }
    return(quality = apply(d, 1, mean))
}
