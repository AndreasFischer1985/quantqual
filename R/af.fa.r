#' Function af.fa
#' 
#' Factor analysis of data.frame.
#' @param data Numeric data.frame.
#' @details Factor analysis of data.frame.
#' @keywords modeling
#' @export
#' @examples
#' af.fa(data.frame(x=rnorm(100),y=rnorm(100)+scale(1:100),z=rnorm(100)+scale(1:100)));plotFA(fa)

af.fa <- function (data, nfactors = NULL, rotate = "varimax", fm = "pa", 
    plot = T, ...) 
{
    library(psych)
    if (is.null(nfactors)) {
        fap = fa.parallel(data, plot = F, show.legend = F)
        nfactors = max(1, fap$nfact)
    }
    fa = fa(cor(data), rotate = rotate, fm = fm, nfactors = nfactors, 
        ...)
    if (plot) 
        plotFA(fa)
    return(fa)
}
