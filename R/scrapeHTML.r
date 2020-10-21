#' Function scrapeHTML
#' 
#' Extracts Information from HTML.
#' @param html A character element containing HTML-code.
#' @param short Logical value specifying whether only lines with verbal information or link should be returned. Defaults to F.
#' @param edit Logical value specifying whether the data.frame should be plotted/edited.
#' @param save Logical value specifying whether the HTML-code should be saved to a csv-file.
#' @param prefix Character value specifying the beginning of the filename (in case of saving). If NULL (default) as.numeric(Sys.time()) is applied.
#' @details Extracts Information from HTML. Returns a data.frame with three columns: the first column contains html-code, the second column contains extracted verbal information, and the third column contains extracted links.
#' @keywords scraping
#' @export
#' @examples
#' scrapeHTML(getHTML())

scrapeHTML <- function (html, short = F, edit = T, save = F, plot = F, prefix = NULL) 
{
    if (length(html) > 1) 
        if (length(dim(html)) > 2) 
            stop("please provide HTML as a character vector!")
        else if (length(dim(html)) > 1) {
            warning("two-dimensional input detected. Only first column is used.")
            html = paste(as.character(html[, 1]), collapse = "   ")
        }
        else html = paste(as.character(html), collapse = "   ")
    strings = gsub("(\n|\t)+", " ", gsub("[ ]+", " ", paste0("<", 
        strsplit(html, "<")[[1]])))
    if (plot) {
        s1 = sort(table(gsub("(<[/]?|(>| ).*)", "", strings)))
        bp(s1[s1 > 2], main2 = "Common Tags")
    }
    info = gsub("^[ ]*$", "", gsub("^<[^<]*>", "", strings))
    links = character(length(info))
    links[grep("href=[\"'].*?[\"']", strings)] = gsub("[\"'].*$", 
        "", gsub("^.*?href=[\"']", "", grep("href=[\"'].*?[\"']", 
            strings, value = T)))
    result = data.frame(entry = as.character(strings), info = as.character(info), 
        links = as.character(links), stringsAsFactors = F)
    if (short) 
        result = result[nchar(as.character(result[, 2])) > 0 | 
            nchar(as.character(result[, 3])) > 0, ]
    if (edit) 
        result = edit(result)
    if (save) 
        write.csv2(data.frame(result), paste0(ifelse(is.character(prefix), 
            prefix, as.numeric(Sys.time())), "_result.csv"))
    return(invisible(result))
}
