#' Function classify
#' @param corpus Character vector containing one document per element.
#' @keywords text mining
#' @export


classify <- function (dat, cat = character(0), cod = character(0), pos = numeric(0), 
    resort = T) 
{
    if (dim(data.frame(dat))[2] == 1) {
        dat = data.frame(dat, 1:length(dat))
        names(dat) = c("code", "category")
    }
    dat = apply(dat, c(1, 2), as.character)
    f1 = function(d, category = cat, codes = cod, positions = pos) {
        d = apply(d, c(1, 2), as.character)
        d[suppressWarnings(d[, 1] == codes), 2] = category
        if (length(codes) > 0) 
            for (i in 1:length(codes)) d[grepl(codes[i], d[, 
                1]), 2] = category
        d[positions, 2] = category
        return(d)
    }
    f2 = function(d) {
        return(d[order(d[, 2], d[, 1]), ])
    }
    f3 = function(d) {
        return(levels(as.factor(d[, 2])))
    }
    dat = f1(dat, cat, cod, pos)
    if (resort) 
        dat = f2(dat)
    message(paste0("Kategorien: ", paste(f3(dat), collapse = ";")))
    return(dat)
}
