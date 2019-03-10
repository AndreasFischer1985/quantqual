#' Function undoTwoColumns
#' 
#' Returns one-column version of two column-text.
#' @param doc Character element with text with two columns (separated by whitespace).
#' @param sepwidth Numeric element specifying the number of whitespaces that are assumed to separate columns. Defaults to 2.
#' @param mid Numeric element specifying the number of characters until the end of the first column. If NULL (default) this number is estimated based on the text.
#' @param trim Logical element speficying whether the text is trimmed before inferring that an empty line is to be treated as/like a pagebreak.
#' @details Returns one-column version of two column-text.
#' @keywords scraping
#' @export


undoTwoColumns <- function (doc, sepwidth = 2, mid = NULL, regex = NULL, trim = T) 
{
    txt = NULL
    pgs = NULL
    if (length(doc) == 1) {
        txt = strsplit(doc, "(\r|\n|\r\n)")[[1]]
        if (is.null(mid)) 
            mid = as.numeric(names(sort(table(unlist(sapply(stringr::str_locate_all(txt, 
                " "), function(x) x[, 1]))), decreasing = T)))[1:sepwidth]
        pgb = which(nchar(quantqual::trim(txt)) == 0)
        if (!is.null(regex)) 
            pgb = grep(regex, txt)
        pgb = NULL
        if (trim & is.null(regex)) {
            nchar2 = nchar(quantqual::trim(txt))
            pgb = which(nchar2 == 0)
            w = which(nchar2 == 0 & nchar(txt) > 0)
            warning(paste("Pagebreaks inserted after timming. Check lines\n", 
                paste(w, collapse = ",")))
        }
        if (!trim & is.null(regex)) 
            pgb = which(nchar(txt) == 0)
        if (!is.null(regex)) 
            pgb = grep(regex, txt)
        pgs = sapply(as.list(1:length(pgb)), function(x) (txt[c(1, 
            pgb)[x]:(pgb)[x]]))
    }
    else {
        pgs = lapply(as.list(doc), function(x) strsplit(x, "(\r|\n|\r\n)")[[1]])
        if (is.null(mid)) 
            mid = as.numeric(names(sort(table(unlist(sapply(stringr::str_locate_all(unlist(doc), 
                " "), function(x) x[, 1]))), decreasing = T)))[1:sepwidth]
    }
    print(mid)
    pg = character(0)
    for (i in 1:length(pgs)) {
        p = pgs[[i]]
        s1 = ""
        s2 = ""
        for (j in 1:length(p)) if (0 == sum(is.na(match(mid, 
            grep(" ", strsplit(p[j], "")[[1]]))))) {
            s1 = c(s1, substring(p[j], 1, mid[1]))
            s2 = c(s2, substring(p[j], mid[1], nchar(p[j])))
        }
        else s1 = c(s1, p[j])
        pg = c(pg, paste(c(s1, s2), collapse = "\n"))
    }
    return(paste(pg, collapse = "\r\n"))
}
