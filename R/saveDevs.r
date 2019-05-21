#' Function saveDevs
#' 
#' Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @param dev Graphical device (currently, pdf, png, bmp, jpg and tiff are supported).
#' @param width width of the devices. If NA (default), width of the devices is left unchanged.
#' @param width height of the devices. If NA (default), height of the devices is left unchanged.
#' @param units Parameter passed to dev.copy. Defaults to "in". Is ignored if dev==pdf.
#' @param res Resolution (dots per inch) of the devices. Defaults to 300. Is ignored if dev==pdf.
#' @details Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @keywords plotting
#' @export
#' @examples
#' saveDevs();

saveDevs <- function (dev = png, prefix = NA, width = NA, height = NA, units = "in", 
    res = 300, ...) 
{
    n = deparse(substitute(dev))
    for (d in dev.list()) {
        dev.set(d)
        if (is.na(prefix)) 
            prefix = ""
        if (is.na(width)) 
            width = (dev.size()[1])
        if (is.na(height)) 
            height = (dev.size()[2])
        if (length(grep("(bmp|png|jpg|tiff)", n)) > 0) 
            dev.copy(dev, paste0(prefix, "Dev", d, ".", n), width = width, 
                height = height, units = units, res = res, ...)
        else if (length(grep("(pdf)", n)) > 0) 
            dev.copy(dev, paste0(prefix, "Dev", d, ".", n), width = width, 
                height = height, ...)
        dev.off()
    }
}
