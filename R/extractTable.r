#' Function extractTable
#' 
#' Extracts table as data.frames from multiple lines of text.
#' @param x A character element containing all the text toextract the table from (or a character vector containing one text-line per element).
#' @param reg.up Regular expression spefifying the top of the table. Defaults to NULL.
#' @param reg.down Regular expression spefifying the bottom of the table. Defaults to NULL.
#' @param reg.left Regular expression spefifying the bottom of the table. Defaults to "^".
#' @param reg.right Regular expression spefifying the bottom of the table. Defaults to "$"
#' @param reg.fix Regular expression spefifying the bottom of the table. Defaults to NULL.
#' @param header Logical value that specifies, whether to treat first row and column as row- and column-names. Defaults to F.
#' @param trim Logical value that specifies, whether to trim entries. Defaults to T.
#' @param convert Logical value that specifies, whether to convert table to numeric (after correcting for notation in case correctNotation is set to T). Please use with care, if table contains non-numeric information. Defaults to T.
#' @param correctNotation Logical value that specifies, whether to correct for the fact that entries use commata instead of dots. Erases all dots in all entries and subsequently replaces commata by dots. Defaults to F.
#' @details Extracts table as data.frames from multiple lines of text. Columns are assumed to be separated by whitespaces that are placed at the same position in each line. Please make sure that headers don't span multiple columns or rows.
#' @keywords scraping
#' @export


extractTable <- function (x, reg.up = NULL, reg.down = NULL, reg.left = "^", 
    reg.right = "$", reg.fix = NULL, header = F, trim = T, convert = F, 
    correctNotation = F) 
{
    if (length(x) == 1) 
        x = strsplit(x, "(\r\n|\r|\n)")[[1]]
    trimIt = function(x) gsub("(^[ ]+|[ ]+$)", "", x)
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
    if (header) {
        colnames(erg) = trim(as.character(erg[1, ]))
        rownames(erg) = trim(as.character(erg[, 1]))
        erg = erg[-1, -1]
    }
    else {
        colnames(erg) = 1:dim(erg)[2]
        rownames(erg) = 1:dim(erg)[1]
    }
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
    if (trim) 
        return(trimIt(erg))
    else return(erg)
}
