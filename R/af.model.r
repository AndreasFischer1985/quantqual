#' Function af.model
#' 
#' Statistical model based on the most relevant predictors as chosen using a stepwise procedure.
#' @param formula A formula specifying the criterion and its relation to all predictors possibly relevant.
#' @param data Numeric data.frame.
#' @param direction Character element specifying the stepwise procedure to be applied (besides "forward", "backward" or "both", possible values are "none", "adaptive", "complete" or "top". It's also possible to add a number after "top" (e.g. "top 3") in order to specify the maximum number of predictors to be examined). Defaults to "top".
#' @param model Function of an approproate class. Defaults to lm.
#' @details This function fits a statistical model based on the most relevant predictors as chosen using a stepwise procedure.
#' @keywords modeling
#' @export
#' @examples
#' af.hclust(data.frame(x=rnorm(100),y=rnorm(100)+scale(1:100),z=rnorm(100)+scale(1:100)));

af.model <- function (formula, data, direction = "top", model = lm, scope = NULL, 
    ...) 
{
    if (is.null(model) | !is.function(model)) 
        model = lm
    if (is.null(scope)) 
        scope = formula
    c = as.character(formula)
    t = attr(terms(formula, data = data), "term.labels")
    if (is.null(direction) | direction == "none") 
        l = model(formula, data, ...)
    if (direction == "adaptive") 
        direction = ifelse(length(t) <= 5, "complete", "forward")
    if (direction == "forward") 
        l = stats::step(object = model(y ~ 1, data, ...), direction = direction, 
            scope = scope)
    if (direction == "backward") 
        l = stats::step(object = model(formula, data, ...), direction = direction, 
            scope = scope)
    if (direction == "both") 
        l = stats::step(object = model(formula, data, ...), direction = direction, 
            scope = scope)
    if (direction == "complete") {
        com = cbind(rep(1:length(t), length(t)))
        for (i in 1:(length(t) - 1)) com = data.frame(com, rep(1:length(t), 
            each = length(t)^i))
        t0 = sapply(1:(length(t)^(dim(com)[2])), function(x) {
            f2 = as.formula(paste(c[2], c[1], paste(t[unlist(com[x, 
                ])], collapse = "+")))
            AIC(model(f2, data, ...))
        })
        formula = as.formula(paste(c[2], c[1], paste(t[unlist(com[which.min(t0), 
            ])], collapse = "+")))
        l = model(formula, data, ...)
    }
    if (sum(grepl("top", direction)) > 0) {
        top = 5
        if (sum(grepl("[0-9]", direction)) > 0) 
            top = as.numeric(gsub("[^0-9]", "", direction))
        top = min(length(t), top)
        com = cbind(1:length(t))
        if (top > 1) 
            for (i in 1:(top - 1)) com = data.frame(com, rep(1:length(t), 
                each = length(t)^i))
        t0 = sapply(1:(length(t)^(dim(com)[2])), function(x) {
            f2 = as.formula(paste(c[2], c[1], paste(t[unlist(com[x, 
                ])], collapse = "+")))
            AIC(model(f2, data, ...))
        })
        formula = as.formula(paste(c[2], c[1], paste(t[unlist(com[which.min(t0), 
            ])], collapse = "+")))
        l = model(formula, data, ...)
    }
    l$call = (parse(text = gsub("= formula", paste("=", deparse(formula)), 
        deparse(l$call))))
    return(l)
}
