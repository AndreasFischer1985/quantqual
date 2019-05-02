#' Function accuracyLDA
#' 
#' For each document in a document-term matrix this function determines the top-terms of the document's top-topic and counts the number of top-terms that are actually present in the document.
#' @param lda Object of class LDA.
#' @param dtm document-term matrix.
#' @param n Number of top-terms to search for in the documents. Defaults to 5.
#' @param type Determines how the results are aggregated. If type==0 (default) the function returns the number of each document's top-topic's n top-terms that are actually present in the document as a vector e; type==1 returns mean(e>0); type==2 returns mean(e==n); type==3 returns mean(e);  type==4 returns median(e>0); type==5 returns median(e==n); type==6 returns median(e);  type==7 returns min(e>0); type==8 returns min(e==n); type==9 returns min(e); 
#' @keywords modeling
#' @export
#' 

accuracyLDA <- function (lda, dtm, n = 5, type = 0) 
{
    vgrepl <- function(...) {
        f = Vectorize(grepl, vectorize.args = "pattern")
        f(...)
    }
    docsTopTerms = rbind(terms(lda, n))[, topics(lda)]
    if (n == 1) 
        dim(docsTopTerms) = c(1, length(docTopTerms))
    e = sapply(1:dim(docsTopTerms)[2], function(i) sum(vgrepl(docsTopTerms[, 
        i], paste(colnames(dtm)[dtm[i, ] > 0], collapse = " ")))/n)
    if (type == 1) 
        return(mean(e > 0))
    else if (type == 2) 
        return(mean(e == n))
    else if (type == 3) 
        return(mean(e))
    else if (type == 4) 
        return(median(e > 0))
    else if (type == 5) 
        return(median(e == n))
    else if (type == 6) 
        return(median(e))
    else if (type == 7) 
        return(min(e) > 0)
    else if (type == 8) 
        return(min(e == n))
    else if (type == 9) 
        return(min(e))
    else return(e)
}
