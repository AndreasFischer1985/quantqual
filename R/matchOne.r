#' Function matchOne
#' 
#' Search for first match to argument pattern within each element of a character vector. Returns corresponding substrings (compare stringr::str_match).
#' @param string Character vector where matches are sought.
#' @param pattern  Character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param value  Logical value. If T (default) returns substrings. Otherwise returns positions.
#' @keywords helper
#' @export
#' @examples
#' matchOne(string=c("Hello world","Hello"),pattern="wo.*")

matchOne <- function (string = NA, pattern = ".*", value = T, ...) 
{
    y = regexpr(pattern, string, ...)
    z = y + attr(y, "match.length") - 1
    l = cbind(y, z)
    t = t(t(substr(string, y, z)))
    if (value == F) 
        return(l)
    else return(t)
}
