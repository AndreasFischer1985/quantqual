#' Function subsetHTML
#' 
#' Extracts a coherent subset of code from HTML-code.
#' @param html A character element containing HTML-code.
#' @param tag Character element specifying the the subsets of interest. Defaults to "div".
#' @param pattern Regular expression further specifying subsets of interest. If NULL (default) equals tag.
#' @param edit Logical value specifying whether the data.frame should be plotted/edited.
#' @param save Logical value specifying whether the HTML-code should be saved to a csv-file.
#' @param prefix Character value specifying the beginning of the filename (in case of saving). Defaults to Sys.Date().
#' @details Extracts a coherent subset of code from HTML-code.
#' @keywords scraping
#' @export
#' @examples
#' subsetHTML(getHTML("https://jobs.meinestadt.de/nuernberg/suche?words=Wissenschaftlicher%20Mitarbeiter",tag="div",pattern="class=\"m-resultListEntries__content\""))

subsetHTML <- function (html, tag = "div", pattern = NULL, edit = F, save = F, 
    plot = F, prefix = Sys.Date(), trim = T) 
{
    start = paste0("<", tag, "[\n\r> ]")
    end = paste0("</", tag, ">")
    if (is.null(pattern)) 
        pattern = start
    strings = gsub("[ ]+", " ", paste0("<", strsplit(html, "<")[[1]]))
    if (plot) {
        s1 = sort(table(gsub("(<[/]?|(>| ).*)", "", strings)))
        bp(s1[s1 > 2], main2 = "Common Tags")
    }
    infos = gsub("^[ ]*$", "", gsub("^<[^<]*>", "", strings))
    links = character(length(infos))
    links[grep("href=[\"'].*?[\"']", strings)] = gsub("[\"'].*$", 
        "", gsub("^.*?href=[\"']", "", grep("href=[\"'].*?[\"']", 
            strings, value = T)))
    loc.pat = grep(pattern, strings)
    loc.div1 = grep(start, strings)
    loc.div2 = grep(end, strings)
    result0 = character(0)
    result1 = character(0)
    result2 = character(0)
    result3 = character(0)
    for (i1 in loc.pat) {
        loc.div3 = sort(c(loc.div1[which(loc.div1 > i1)], loc.div2[which(loc.div2 > 
            i1)]))
        i2 = i1
        for (i in loc.div3) {
            i2 = i
            if (length(loc.div1[which(loc.div1 > i1 & loc.div1 <= 
                i)]) < length(loc.div2[which(loc.div2 > i1 & 
                loc.div2 <= i)])) 
                break
        }
        string = paste(strings[i1:i2], collapse = "\n")
        info = paste(infos[i1:i2], collapse = "\n")
        link = paste(links[i1:i2], collapse = "\n")
        text = gsub("^( )*(\n)*( )*(\n)*", "", gsub("(\n)+( )*(\n)*", 
            "\n", gsub("<.*?>", "", string)))
        if (trim == T) {
            result0 = c(result0, gsub("[\n]+", "\n", quantqual::trim(string)))
            result1 = c(result1, gsub("[\n]+", "\n", quantqual::trim(text)))
            result2 = c(result2, gsub("[\n]+", "\n", quantqual::trim(info)))
            result3 = c(result3, gsub("[\n]+", "\n", quantqual::trim(link)))
        }
        else {
            result0 = c(result0, (string))
            result1 = c(result1, (text))
            result2 = c(result2, (info))
            result3 = c(result3, (link))
        }
    }
    result = data.frame(entry = result0, info = result2, links = result3, 
        text = result1)
    if (edit) 
        result = edit(result)
    if (save) {
        write.csv2(data.frame(result), paste0(prefix, "_", round(10000 * 
            rnorm(1)), "_result.csv"))
    }
    return(invisible(result))
}
