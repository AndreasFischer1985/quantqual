#' Function nnets
#' 
#' Fits multiple initializations of nnet and returns a sorted list of nnet-objects.
#' @param data.train data.frame containing training data.
#' @param output index or name of criterion in training data.
#' @keywords modeling
#' @export
#' @examples
#'  

nnets <- function (data.train, output = NULL, rep.nnet = 10, seed = 0, 
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
    out <- lapply(c(1:rep.nnet), function(x) nnet::nnet(output ~ 
        ., data = data.train, size = size, decay = decay, linout = linout, 
        trace = trace, ...))
    out <- out[order(sapply(out, function(x) cor(predict(x), 
        data.train$output, use = "pairwise")), decreasing = T)]
    if (plot) 
        af.sensitivity(out[[1]], data.train[colnames(data.train) != 
            "output"], data.train["output"])
    return(out)
}
