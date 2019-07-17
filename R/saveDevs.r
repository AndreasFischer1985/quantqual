#' Function saveDevs
#' 
#' Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @param prefix Character vector specifying a sequence of characters to begin the files' names with.
#' @param width width of the devices. If NA (default), width of the devices is left unchanged.
#' @param width height of the devices. If NA (default), height of the devices is left unchanged.
#' @param dev Graphical device (currently, pdf, png, bmp, jpg and tiff are supported).
#' @param units Parameter passed to dev.copy. Defaults to "in". Is ignored if dev==pdf.
#' @param res Resolution (dots per inch) of the devices. Defaults to 300. Is ignored if dev==pdf.
#' @param mess Logical value specifying if a message should be printed after saving the devices. Defaults to T.
#' @param close Logical value specifying whether to close devices after saving them.
#' @details Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @keywords plotting
#' @export
#' @examples
#' saveDevs();

saveDevs <- function (prefix = NA, width = NA, height = NA, dev = png, units = "in", 
    res = 300, mess = T, close = F, ...) 
{
    n = deparse(substitute(dev))
    l = dev.list()
    if (length(prefix) != length(l)) 
        prefix = rep(prefix[1], length(l))
    if (length(width) != length(l)) 
        width = rep(width[1], length(l))
    if (length(height) != length(l)) 
        height = rep(height[1], length(l))
    for (i in 1:length(l)) {
        d = l[i]
        dev.set(d)
        width1 = width[i]
        height1 = height[i]
        prefix1 = prefix[i]
        if (is.na(width1)) 
            width1 = (dev.size()[1])
        if (is.na(height1)) 
            height1 = (dev.size()[2])
        if (is.na(prefix1)) 
            prefix1 = ""
        if (length(grep("(bmp|png|jpg|tiff)", n)) > 0) 
            dev.copy(dev, paste0(prefix1, "Dev", d, ".", n), 
                width = width1, height = height1, units = units, 
                res = res, ...)
        else if (length(grep("(pdf)", n)) > 0) 
            dev.copy(dev, paste0(prefix1, "Dev", d, ".", n), 
                width = width1, height = height1, ...)
        else dev.copy(dev, paste0(prefix1, "Dev", d, ".", n), 
            width = width1, height = height1, ...)
        dev.off()
    }
    if (close) 
        graphics.off()
    if (mess) 
        message(ifelse(length(l) > 1, paste0(length(l), " devices saved to\n", 
            getwd()), paste0(length(l), " device saved to\n", 
            getwd())))
}
