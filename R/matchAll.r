#' Function matchAll
#' 
#' Search for matches to argument pattern within each element of a character vector. Returns corresponding substrings (compare stringr::str_match_all).
#' @param string Character vector where matches are sought.
#' @param pattern  Character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param value  Logical value. If T (default) returns substrings. Otherwise returns positions.
#' @keywords helper
#' @export
#' @examples
#' matchAll(string=c("Hello world","Hello"),pattern="wo.*")

matchAll <- function (string = NA, pattern = ".*", value = T, ...) 
{
    y = gregexpr(pattern, string, ...)
    l = (lapply(y, function(x) rbind(x, attr(x, "match.length"))))
    if (value == F) 
        return(lapply(l, t))
    else t = lapply(1:length(l), function(i) {
        l1 = l[[i]]
        cbind(apply(l1, 2, function(x1) if (x1[2] > -1) 
            return(substr(string[[i]], x1[1], x1[1] + x1[2] - 
                1))
        else return(matrix(NA, ncol = 0, nrow = 0))))
    })
    return(t)
}
