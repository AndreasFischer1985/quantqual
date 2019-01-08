#' Function getFile
#' 
#' Downloads File from URL.
#' @param url A valid URL.
#' @param filename Character value specifying the path to save the downloaded file to.
#' @details Downdloads File from URL.
#' @keywords scraping
#' @export
#' @examples
#' getFile()

getFile <- function (url = "http://www.bamf.de/SharedDocs/Anlagen/DE/Publikationen/Flyer/flyer-schluesselzahlen-asyl-halbjahr-2018.pdf?__blob=publicationFile", 
    filename = NULL, mode = "wb") 
{
    if (is.null(filename)) 
        filename = paste0(gsub("[/?.:]", "", url), ".pdf")
    message(paste0("Trying to get file from ", url))
    download.file(url, filename, mode = mode)
    if (sum(grepl("[.](txt|pdf|doc|docx)", filename)) > 0) 
        return(readtext::readtext(filename)[, 2])
}
