#' Function codeByRegex
#' 
#' Adds code tags to certain passages of a character vector.
#' @param corpus Character vector containing one document per element.
#' @param passage Character element containing a regular expression that describes the passages that are to be coded.
#' @param coding Character element containing the label of the code tag. Defaults to "code".
#' @param context Numeric element specifying the context that is to be coded: 0 for coding only words, 2 for coding sentences, 3 for coding whole documents. Defaults to 0
#' @param mess Logical element. If T (default) there will be a message containing the coded corpus.
#' @param assign Logical element. If T the corpus will be saved in the global environment. Defaults to F.
#' @param subcorpus Numeric vector specifying which documents of the corpus to code. If NULL (default) the whole corpus will be coded.
#' @param partial Logical element specifying whether words that contain the passage (without being identical to it) should also be coded. Defaults to F.
#' @param prepare Logical element specifying whether to exclude "\n" or "\r" from the corpus before coding. Defaults to T.
#' @details Adds code tags to certain passages of a character vector.
#' @keywords text mining
#' @export
#' @examples
#' codeByPassage("hello world hello world hello world hello world","hello","code1")

codeByRegex <- function (corpus, passage, coding = "code", context = 0, mess = T, 
    assign = F, subcorpus = NULL, partial = F, prepare = T) 
{
    if (prepare) 
        corpus = gsub("(\n|\r)", " ", corpus)
    context = as.numeric(context)
    addChars = ""
    if (partial == F) 
        addChars = "\\b"
    if (context == 0) {
        passage = paste0(addChars, passage, addChars)
    }
    if (context == 1) {
        passage = paste0("[^.;?!]*", addChars, passage, addChars, 
            "(.*?)([.;?!]+|$)")
    }
    if (context == 2) {
        passage = paste0("(.*?)", addChars, passage, addChars, 
            "(.*?)")
    }
    if (is.null(subcorpus)) 
        seq = 1:length(corpus)
    if (!is.null(subcorpus)) 
        seq = subcorpus
    for (i in seq) for (j in 1:length(passage)) if (!is.na(stringr::str_match(corpus, 
        passage[j])[i, 1])) 
        corpus[i] = gsub(passage[j], paste0("<", coding[j], ">", 
            stringr::str_match(corpus, passage[j])[i, 1], "</", 
            coding[j], ">"), corpus[i])
    if (mess) 
        message(paste0(1:length(corpus), ":\n", corpus, collapse = "\n"))
    if (assign) 
        assign(deparse(substitute(corpus)), corpus, envir = .GlobalEnv)
    return(corpus)
}
