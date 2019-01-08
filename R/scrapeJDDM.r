#' Function scrapJDDM
#' 
#' Downloads data and metadata from the database of the Journal of Dynamic Decision Making to the working directory.
#' @param wd Pathname for the working directory to save the files into.
#' @details Downloads data and metadata from the database of the Journal of Dynamic Decision Making to the working directory. Returns a data.frame containing the relevant information.
#' @keywords JDDM
#' @export
#' @examples
#' s=scrapeJDDM();

scrapeJDDM <- function (wd = NULL, saveHTML = F) 
{
    library(stringr)
    g = getwd()
    if (!is.null(wd)) 
        setwd(wd)
    tryCatch({
        load("html0.RData")
    }, error = function(c) {
    })
    if (!exists("html0")) {
        html0 = paste(readLines("https://journals.ub.uni-heidelberg.de/index.php/jddm/search/titles", 
            encoding = "UTF-8"), collapse = "\n")
        if (saveHTML) 
            save(html0, file = "html0.RData")
    }
    links = str_match_all(html0, "<a href=\\\"(.*)?\\\" class=\\\"file\\\">PDF</a>")[[1]][, 
        2]
    article.ids = gsub(".*view/", "", links)
    if (F) {
        files = list.files(pattern = "(.*?).pdf")
        files
        if (length(files) == 0) {
            html = paste(readLines("https://journals.ub.uni-heidelberg.de/index.php/jddm/search/titles", 
                encoding = "UTF-8"), collapse = "\n")
            links = str_match_all(html, "<a href=\\\"(.*)?\\\" class=\\\"file\\\">PDF</a>")[[1]][, 
                2]
            article.ids = gsub(".*view/", "", links)
            downloadlinks = paste0("https://journals.ub.uni-heidelberg.de/index.php/jddm/article/download/", 
                article.ids)
            for (i in 1:length(downloadlinks)) {
                url = downloadlinks[i]
                download.file(url, paste0(gsub("(^.*download/|/)", 
                  "", url), ".pdf"), mode = "wb")
            }
            files = list.files(pattern = "(.*?).pdf")
        }
        corpus = character(0)
        for (i in 1:length(files)) corpus = c(corpus, paste(readPDF(control = list(text = "-layout"))(elem = list(uri = files[i]), 
            language = "en", id = "id1"), collapse = "\n\n"))
        corpus = gsub("  ", "", corpus)
    }
    s = strsplit(html0, "(<td)")[[1]]
    s = grep("style=\\\"padding-left: 30px;font-style: italic;\\\"", 
        s, value = T)
    all.authors = gsub("(^(.)*\n\\t\\t\\t\\t\\t|\\t\\t\\t<(.)*$|\t\t\t\t\t)", 
        "", s)
    authors = gsub(",(.*)$", " et al.", gsub("(^(.)*\n\\t\\t\\t\\t\\t|\\t\\t\\t<(.)*$|\t\t\t\t\t)", 
        "", s))
    authors[grep("Fischer(.*)Holt(.*)Funke", all.authors)] = "Editoral"
    s = strsplit(html0, "(<td)")[[1]]
    years = gsub("((.)*Vol [1-9] \\(|\\)(.)*)", "", grep("Vol [1-9] \\(201[1-9]\\)", 
        s, value = T))
    article.labels = paste0(authors, " (", years, ")")
    names(article.labels) = article.ids
    tryCatch({
        load("html.RData")
    }, error = function(c) {
    })
    if (!exists("html")) {
        html = list()
        for (i in 1:length(article.ids)) {
            html[as.character(article.ids[i])] = paste(readLines(paste0("https://journals.ub.uni-heidelberg.de/cgi-bin/oastats.cgi?repo=ojs;from_date=2015-09-29%2021:39:27;id=jddm:", 
                gsub("/.*", "", article.ids[i]), ";lang=ende;overlay=1"), 
                encoding = "UTF-8"), collapse = "\n")
            message(article.ids[i])
        }
        if (saveHTML) 
            save(html, file = "html.RData")
    }
    html = unlist(html)
    current.year = as.numeric(gsub("-.*", "", (Sys.Date())))
    span = current.year - 2015 + 1
    results = list()
    for (i in 1:length(html)) {
        months = paste(paste0(">", c("Jan", "Feb", "Mar", "Apr", 
            "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
            "Dec"), "<"), collapse = "|")
        df = data.frame(c("Intro", str_match_all(html[i], months)[[1]]), 
            strsplit(as.character(html[i]), months))
        df[, 2] = gsub("<table class=\\\"stats\\\"><tr><th>201[0-9]</th>", 
            "", df[, 2])
        numbers = sapply(str_match_all(df[, 2], ">[0-9]+<"), 
            function(x) as.numeric(gsub("[<>]", "", c(x[1:2]))))[, 
            -1]
        if (is.null(dim(numbers))) 
            numbers = as.matrix(numbers)
        colnames(numbers) = gsub("[<>]", "", df[-1, 1])
        rownames(numbers) = c("Downloads", "Frontdoor")
        mo = dim(numbers)[2]
        numbers = cbind(numbers, matrix(NA, nrow = 2, ncol = 24 - 
            mo))
        colnames(numbers) = paste(colnames(numbers), c(rep(current.year, 
            12), rep(current.year - 1, 12)))
        st = str_match_all(df[, 2], ">[0-9]+<")
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
            article.ids), downloads, frontdoor, statistics.htmls = unlist(html))
    if (any(rowSums(is.na(downloads[, (current.year - 2013):dim(downloads)[2]])) > 
        11) | any(rowSums(is.na(frontdoor[, (current.year - 2013):dim(frontdoor)[2]])) > 
        11)) 
        warning("only a subset of papers is returned. please try again next month")
    df = df[(rowSums(is.na(downloads[, (current.year - 2013):dim(downloads)[2]])) < 
        12 & rowSums(is.na(frontdoor[, (current.year - 2013):dim(frontdoor)[2]])) < 
        12), ]
    return(df)
}
