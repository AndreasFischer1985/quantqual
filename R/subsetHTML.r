#' Function subsetHTML
#' 
#' Extracts a coherent subset of code from HTML-code.
#' @param html A character element containing HTML-code.
#' @param pattern Regular expression specifying the tag to be used for subsetting.
#' @param start Regular expression. Defaults to "<div".
#' @param end Regular expression. Defaults to "</div>".
#' @param save Logical value specifying whether the HTML-code should be saved to a csv-file.
#' @param prefix Character value specifying the beginning of the filename (in case of saving). Defaults to Sys.Date().
#' @details Extracts a coherent subset of code from HTML-code.
#' @keywords scraping
#' @export
#' @examples
#' subsetHTML(getHTML("https://jobs.meinestadt.de/nuernberg/suche?words=Wissenschaftlicher%20Mitarbeiter"))

subsetHTML <- function (html, pattern = "class=\"m-resultListEntries__content\"", 
    start = "<div", end = "</div>", save = T, prefix = Sys.Date()) 
{
    strings = gsub("[ ]+", " ", paste0("<", strsplit(html, "<")[[1]]))
    loc.pat = grep(pattern, strings)
    loc.div1 = grep(start, strings)
    loc.div2 = grep(end, strings)
    result = character(0)
    for (i1 in loc.pat) {
        loc.div3 = sort(c(loc.div1[which(loc.div1 > i1)], loc.div2[which(loc.div2 > 
            i1)]))
        i2 = i1
        for (i in loc.div3) if (length(loc.div1[which(loc.div1 > 
            i1 & loc.div1 <= i)]) <= length(loc.div2[which(loc.div2 > 
            i1 & loc.div2 <= i)])) {
            i2 = i
            break
        }
        string = paste(strings[i1:i2], collapse = "\n")
        text = gsub("^( )*(\n)*( )*(\n)*", "", gsub("(\n)+( )*(\n)*", 
            "\n", gsub("<.*?>", "", string)))
        result = c(result, text)
    }
    if (save) 
        write.csv2(data.frame(result), paste0(prefix, "_", round(10000 * 
            rnorm(1)), "_result.csv"))
    return(result)
}
