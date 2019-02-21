#' Function writeReport
#' 
#' Generates and renders rmarkdown-documents.
#' @param code List of functions containing the code to be documented in the document.
#' @param intro Character vector containing the text to be plotted above each chunk of code.
#' @param options Character vector containing the options to be applied for each chunk of code.
#' @param author Character element specifying the author of the document. Defaults to "`r Sys.Date()`".
#' @param title Character element specifying the document title.
#' @param format Character element specifying the output format (e.g., "word_document", "html_document" or  "pdf_document").
#' @param date Character element specifying the date of the document. Defaults to "Mr. X".
#' @details Generates and renders rmarkdown-documents.
#' @keywords plotting
#' @export
#' @examples
#' writeReport(data.frame(group1=c(variable1=1,variable2=2),group2=c(variable1=3,variable2=4)),horiz=T,main="Example")

writeReport <- function (code = list(function() {
    library(quantqual)
    plotMAT()
    bp()
    flowerplot()
    spiderplot()
}, function() {
    "Hello world"
}), intro = c("# Report\nThe research report was generated using the \"quantqual\" package for GNU R."), 
    options = "fig.height=7, fig.width=7", author = NULL, title = "Research Report", 
    format = "word_document", date = "`r Sys.Date()`") 
{
    if (!is.list(intro)) 
        intro = list(intro)
    if (!is.list(options)) 
        options = list(options)
    if (!is.list(code)) 
        code = list(code)
    if (length(intro) == 1) 
        intro = list(intro[[1]], rep("", (length(code) - 1)))
    if (length(options) == 1) 
        options = as.list(rep(options[[1]], length(code)))
    library(knitr)
    if (is.null(title) | is.null(format)) 
        stop("Please provide title and format of the file to be created.")
    c = paste0("---\n", "title: \"", title, "\"\n")
    if (!is.null(author)) 
        c = paste0(c, "author:\n", "- ", author, "\n")
    if (!is.null(date)) 
        c = paste0(c, "date: \"", date, "\"\n")
    c = paste0(c, "output: ", "rmarkdown::", format, "\n", "---\n  \n")
    for (i in 1:length(code)) c = paste0(c, intro[[i]], "\n```{r ", 
        options[[i]], "}\n", paste(deparse(code[[i]])[3:(length(deparse(code[[i]])) - 
            1)], collapse = "\n"), "\n```\n  \n")
    message(code)
    title = paste0(gsub("\\W", "", title))
    write(c, file = paste0(title, ".Rmd"))
    rmarkdown::render(paste0(title, ".Rmd"))
    return(c)
}
