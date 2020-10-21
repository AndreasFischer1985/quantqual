#' Function saveDevs
#' 
#' Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @param filename Character vector containing file names.
#' @param width Width of the devices. If NA (default), width of the devices is left unchanged.
#' @param height Height of the devices. If NA (default), height of the devices is left unchanged.
#' @param dev Graphical device (currently, pdf, win.metafile, png, bmp, jpg and tiff are supported).
#' @param units Parameter passed to dev.copy. Defaults to "in". Is ignored if dev==pdf.
#' @param res Resolution (dots per inch) of the devices. Defaults to 300. Is ignored if dev==pdf.
#' @param mess Logical value specifying if a message should be printed after saving the devices. Defaults to T.
#' @param close Logical value specifying whether to close devices after saving them.
#' @details Saves each element of dev.list() to a graphical device such as pdf, png or similar.
#' @keywords plotting
#' @export
#' @examples
#' saveDevs();

saveDevs <- function (filename = NA, width = NA, height = NA, dev = png, 
    units = "in", res = 300, mess = T, close = F, ...) 
{
    n = deparse(substitute(dev))
    n[n == "win.metafile"] = "emf"
    l = dev.list()
    if (length(l) == 0) {
        warning("No device open.")
        return(invisible())
    }
    if (length(filename) != length(l)) 
        filename = rep(filename[1], length(l))
    if (length(width) != length(l)) 
        width = rep(width[1], length(l))
    if (length(height) != length(l)) 
        height = rep(height[1], length(l))
    filename[is.na(filename)] = ""
    if (sum(table(match(filename, levels(as.factor(filename)))) > 
        1) > 0) 
        filename = paste(filename, 1:length(filename))
    if (filename[1] == "" & length(filename) == 1) 
        filename = "Plot"
    filename[grepl(paste0("[.]", n, "$"), filename) == F] = paste0(filename[grepl(paste0("[.]", 
        n, "$"), filename) == F], ".", n)
    for (i in 1:length(l)) {
        d = l[i]
        dev.set(d)
        width1 = width[i]
        height1 = height[i]
        filename1 = filename[i]
        if (is.na(width1)) 
            width1 = (dev.size()[1])
        if (is.na(height1)) 
            height1 = (dev.size()[2])
        if (length(grep("(bmp|png|jpg|tiff)", n)) > 0) 
            dev.copy(dev, filename1, width = width1, height = height1, 
                units = units, res = res, ...)
        else if (length(grep("(pdf|emf)", n)) > 0) 
            dev.copy(dev, filename1, width = width1, height = height1, 
                ...)
        else dev.copy(dev, filename1, width = width1, height = height1, 
            ...)
        dev.off()
    }
    if (close) 
        graphics.off()
    if (mess) 
        message(ifelse(length(l) > 1, paste0(length(l), " devices saved to\n", 
            getwd()), paste0(length(l), " device saved to\n", 
            getwd())))
}
