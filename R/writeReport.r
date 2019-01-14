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
}), intro = c("# Section 1\nThis is an exemplary research report demonstrating the default output of some quantqual-functions.", 
    "# Section 2"), options = "fig.height=7, fig.width=7", author = "Mr. X", 
    title = "Research Report", format = "word_document", date = "`r Sys.Date()`") 
{
    intro = as.list(intro)
    options = as.list(options)
    code = as.list(code)
    if (length(intro) == 1) 
        intro = list(intro[[1]], rep("", (length(code) - 1)))
    if (length(options) == 1) 
        options = as.list(rep(options[[1]], length(code)))
    library(knitr)
    c = paste0("---\n", "title: \"", title, "\"\n", "author:\n", 
        "- ", author, "\n", "date: \"", date, "\"\n", "output: ", 
        "rmarkdown::", format, "\n", "---\n  \n")
    for (i in 1:length(code)) c = paste0(c, intro[[i]], "\n```{r ", 
        options[[i]], "}\n", paste(deparse(code[[i]])[3:(length(deparse(code[[i]])) - 
            1)], collapse = "\n"), "\n```\n  \n")
    message(code)
    title = paste0(gsub("[/?.:]", "", title))
    write(c, file = paste0(title, ".Rmd"))
    rmarkdown::render(paste0(title, ".Rmd"))
    return(c)
}
