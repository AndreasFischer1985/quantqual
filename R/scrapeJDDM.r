#' Function scrapeJDDM
#' 
#' Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @param plot Logical value specifying wether to plot data on reads and downloads.
#' @details Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @keywords scraping
#' @export
#' @examples
#' s=scrapeJDDM();

scrapeJDDM <- function (plot = T, urls = paste0("https://journals.ub.uni-heidelberg.de/index.php/jddm/issue/view/", 
    c(3269, 2836, 2269, 3694, 4432)), url2 = "https://journals.ub.uni-heidelberg.de/cgi-bin/oastats.cgi?repo=ojs;from_date=2015-09-29%2021:39:27;id=jddm:") 
{
    article.ids = character(0)
    all.authors = character(0)
    authors = character(0)
    years = character(0)
    article.labels = character(0)
    for (url in urls) {
        html0 = gsub("(\t|\n)", "", paste(readLines(url, encoding = "UTF-8"), 
            collapse = "\n"))
        links = matchAll(html0, "https://journals.ub.uni-heidelberg.de/index.php/jddm/article/view/[0-9/]*\\\">PDF")[[1]][, 
            1]
        article.ids = c(article.ids, gsub("(.*view/|\\\">PDF)", 
            "", links))
        all.authors = c(all.authors, gsub("</div>.*", "", strsplit(html0, 
            "(\"authors\\\">)")[[1]])[-1])
        authors = paste0(gsub(",.*", "", all.authors), " et al.")
        authors[grep("Fischer(.*)Holt(.*)Funke", all.authors)] = "Editoral"
        years = c(years, rep(gsub("(.*[(]|[)])", "", matchOne(html0, 
            "<title>Vol [0-9]* \\([0-9]*\\)"))[, 1], length(links)))
    }
    article.labels = paste0(authors, " (", years, ")")
    names(article.labels) = article.ids
    html = list()
    for (i in 1:length(article.ids)) {
        html[as.character(article.ids[i])] = paste(readLines(paste0(url2, 
            gsub("/.*", "", article.ids[i]), ";lang=ende;overlay=1"), 
            encoding = "UTF-8"), collapse = "\n")
        message(article.ids[i])
    }
    html = unlist(html)
    current.year = as.numeric(gsub("-.*", "", (Sys.Date())))
    span = current.year - 2015 + 1
    results = list()
    for (i in 1:length(html)) {
        months = paste(paste0(">", c("Jan", "Feb", "Mar", "Apr", 
            "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
            "Dec"), "<"), collapse = "|")
        df = data.frame(c("Intro", matchAll(html[i], months)[[1]]), 
            strsplit(as.character(html[i]), months))
        df[, 2] = gsub("<table class=\\\"stats\\\"><tr><th>20[0-9][0-9]</th>", 
            "", df[, 2])
        numbers = sapply(matchAll(df[, 2], ">[0-9]+<"), function(x) as.numeric(gsub("[<>]", 
            "", c(x[1:2]))))[, -1]
        if (is.null(dim(numbers))) 
            numbers = as.matrix(numbers)
        colnames(numbers) = gsub("[<>]", "", df[-1, 1])
        rownames(numbers) = c("Downloads", "Frontdoor")
        mo = dim(numbers)[2]
        numbers = cbind(numbers, matrix(NA, nrow = 2, ncol = 24 - 
            mo))
        colnames(numbers) = paste(colnames(numbers), c(rep(current.year, 
            12), rep(current.year - 1, 12)))
        st = matchAll(df[, 2], ">[0-9]+<")
        prior = as.numeric(gsub("[<>]", "", st[[length(st)]][-c(1:2), 
            1]))
        prior = c(prior, rep(NA, 2 * (as.numeric(gsub("-.*", 
            "", (Sys.Date()))) - 2014) - length(prior)))
        numbers = cbind(numbers, t(data.frame(prior[seq(1, length(prior), 
            2)], prior[seq(1, length(prior), 2) + 1])))
        colnames(numbers)[25:(24 + span)] = as.numeric(gsub("-.*", 
            "", (Sys.Date()))):2015
        results[[as.character(article.ids[i])]] = numbers
    }
    m = matrix(ncol = (24 + span))
    for (i in 1:length(results)) m = rbind(m, results[[i]])
    m = m[-1, ]
    rownames(m) = paste(rownames(m), rep(article.ids, each = 2))
    if (as.numeric(gsub("(^[0-9]*-|-[0-9]*$)", "", Sys.Date())) < 
        12) {
        shift = which(!is.na(m[, 1]))
        m[shift, 13:24] = m[shift, 1:12]
        m[shift, 1:12] = NA
        m = m[, first(which(dim(m)[1] != colSums(is.na(m)))):dim(m)[2]]
    }
    names(article.labels) = article.ids
    current.year = as.numeric(gsub("-.*", "", (Sys.Date())))
    years.JDDM = (as.numeric(gsub("-.*", "", (Sys.Date()))) - 
        2014)
    entryNo = 2 * 12 + years.JDDM - 2
    downloads = m[seq(1, dim(m)[1], 2), dim(m)[2]:1]
    frontdoor = m[seq(1, dim(m)[1], 2) + 1, dim(m)[2]:1]
    downloads.per.year = m[seq(1, dim(m)[1], 2), dim(m)[2]:1][, 
        1:years.JDDM]
    frontdoor.per.year = m[seq(1, dim(m)[1], 2) + 1, dim(m)[2]:1][, 
        1:years.JDDM]
    frontdoor.per.year[is.na(frontdoor.per.year)] = 0
    downloads.per.year[is.na(downloads.per.year)] = 0
    df = data.frame(article.ids, article.labels, all.authors, 
        authors, years, download.link = paste0("https://journals.ub.uni-heidelberg.de/index.php/jddm/article/download/", 
            article.ids), downloads, frontdoor)
    if (F) {
        if (any(rowSums(is.na(downloads[, (current.year - 2013):dim(downloads)[2]])) > 
            11) | any(rowSums(is.na(frontdoor[, (current.year - 
            2013):dim(frontdoor)[2]])) > 11)) 
            warning("only a subset of papers is returned. please try again next month")
        df = df[(rowSums(is.na(downloads[, (current.year - 2013):dim(downloads)[2]])) < 
            12 & rowSums(is.na(frontdoor[, (current.year - 2013):dim(frontdoor)[2]])) < 
            12), ]
    }
    if (plot) {
        downloads2 = downloads[, colnames(downloads) != as.character(current.year - 
            1) & colnames(downloads) != as.character(current.year)]
        rownames(downloads2) = article.labels
        downloads2 = downloads2[order(rowSums(downloads2, na.rm = T), 
            decreasing = T)[1:min(dim(downloads2)[1], 25)], ]
        dev.new(width = 10, height = 7)
        plotMAT(downloads2, main = "Cumulation of Downloads", 
            cumsum = T)
        frontdoor2 = frontdoor[, colnames(frontdoor) != as.character(current.year - 
            1) & colnames(frontdoor) != as.character(current.year)]
        rownames(frontdoor2) = article.labels
        frontdoor2 = frontdoor2[order(rowSums(frontdoor2, na.rm = T), 
            decreasing = T)[1:min(dim(frontdoor2)[1], 25)], ]
        dev.new(width = 10, height = 7)
        plotMAT(frontdoor2, main = "Cumulation of Reads", cumsum = T)
    }
    return(df)
}
