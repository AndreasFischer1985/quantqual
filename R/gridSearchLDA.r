#' Function gridSearchLDA
#' @keywords modeling
#' @export


gridSearchLDA <- function (dtm, k = NULL, alpha = NULL, delta = NULL, grid = NULL, 
    fun = BIC, best = T, plot = T, ...) 
{
    grid.ldas = NULL
    if (is.null(k) & is.null(alpha) & is.null(delta) & is.null(grid)) 
        grid = expand.grid(k = c(2, 3, 4, 5, 6), alpha = c(0.001, 
            0.01, 0.1, 0.5, 1), delta = c(0.1))
    if (is.null(grid)) 
        grid = expand.grid(k = k, alpha = alpha, delta = delta)
    if (length(grep("parallel", (installed.packages()[, "Package"]))) > 
        0) {
        mc.cores = parallel::detectCores()
        cl <- parallel::makeCluster(mc.cores)
        parallel::setDefaultCluster(cl)
        parallel::clusterExport(varlist = c("dtm", "grid"), envir = environment())
        grid.ldas <- parallel::parLapply(X = 1:dim(grid)[1], 
            fun = function(x) {
                n = 1
                topicmodels::LDA(dtm, k = grid[x, "k"], method = "Gibbs", 
                  control = list(alpha = grid[x, "alpha"], delta = grid[x, 
                    "delta"], ...))
            })
        parallel::stopCluster(cl)
    }
    else {
        grid.ldas = sapply(1:dim(grid)[1], function(x) {
            n = 1
            topicmodels::LDA(x = dtm, k = grid[x, "k"], method = "Gibbs", 
                control = list(alpha = grid[x, "alpha"], delta = grid[x, 
                  "delta"], ...))
        })
    }
    quality = sapply(grid.ldas, fun)
    best.lda = grid.ldas[[which.max(quality)]]
    message(paste("Best:\n", paste(paste(names(unlist(grid[which.max(quality), 
        ])), "=", unlist(grid[which.max(quality), ])), collapse = ",")))
    if (plot) 
        plotLDA(best.lda)
    if (best) 
        return(best.lda)
    else return(grid.ldas)
}
