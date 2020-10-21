#' Function plotLM
#' 
#' Plots regression coefficiens based on lm object.
#' @param lm Object of class lm.
#' @details Plots regression coefficiens based on lm object.
#' @keywords plotting
#' @export
#' @examples
#' plotLM()

plotLM <- function (lm = NULL, ...) 
{
    if (is.null(lm)) 
        lm = lm(y ~ ., data = data.frame(y = rnorm(100), x = rnorm(100), 
            z = rnorm(100)))
    data = data.frame(coef(lm), confint(lm))
    data = (data + 1)/2
    spiderplot(x = data[, 1], lower = data[, 2], upper = data[, 
        3], names = names(coef(lm)), ...)
    title(sub = paste0(expression(R^2), " = ", round(summary(lm)$r.squared, 
        2)))
    invisible(summary(lm))
}
