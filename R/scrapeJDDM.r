#' Function scrapeJDDM
#' 
#' Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @param plot Logical vakue specifying wether to plot data on reads and downloads.
#' @details Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @keywords scraping
#' @export
#' @examples
#' s=scrapeJDDM();

scrapeJDDM <- function (x, reg.up = NULL, reg.down = NULL, reg.left = "^", 
    reg.right = "$", reg.fix = NULL, convert = F, correctNotation = T) 
{
    trim = function(x) gsub("(^[ ]+|[ ]+$)", "", x)
    if (!is.null(reg.up)) {
        lines = (grep(reg.up, x)[1]):(grep(reg.down, x)[1] - 
            1)
        x = x[lines]
        rows = (unlist(gregexpr(reg.left, x[1]))[1]):(unlist(gregexpr(reg.right, 
            x[1]))[1])
        x = substr(x, min(rows), max(rows))
    }
    cells = x[nchar(x) > 0 & !(grepl("^[ ]+$", x))]
    t = data.frame(sapply(cells, function(y) paste0(y, paste(rep(" ", 
        max(nchar(cells)) - nchar(y)), collapse = ""))))
    t2 = strsplit(as.character(t[, 1]), "")
    d1 = data.frame(t2[[1]])
    for (i in 2:length(t2)) d1 = data.frame(d1, t2[[i]])
    t2 = t(d1)
    w = c(which(colSums(t2 == " ") == max(colSums(t2 == " "))), 
        dim(t2)[2])
    e = list()
    for (i in 1:length(w)) e[[i]] = apply(data.frame(t2[, ifelse(i == 
        1, 1, w[i - 1]):w[i]]), 1, function(x) paste(x, collapse = ""))
    erg = data.frame(e[[1]])
    for (i in 2:length(w)) erg = apply(data.frame(erg, e[[i]]), 
        2, as.character)
    colnames(erg) = trim(as.character(erg[1, ]))
    rownames(erg) = trim(as.character(erg[, 1]))
    erg = erg[-1, -1]
    erg = erg[, !colSums(erg == "  ") == max(colSums(erg == "  "))]
    rn = rownames(erg)
    if (correctNotation) 
        erg = apply(erg, 2, function(x) gsub("[%.]", "", x))
    if (correctNotation) 
        erg = apply(erg, 2, function(x) gsub("[,]", ".", x))
    if (!is.null(reg.fix)) 
        erg = gsub(reg.fix, "", erg)
    if (convert) {
        erg = apply(erg, 2, as.numeric)
        rownames(erg) = rn
    }
    erg
}
