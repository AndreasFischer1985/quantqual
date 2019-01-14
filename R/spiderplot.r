#' Function spiderplot
#' 
#' Plots a radar chart based on a numeric vector.
#' @param x Numeric vector containing the values to be displayed. Values should be between 0 and 1.
#' @param lower Numeric vector of the same length as x, containing the values to be displayed as an lower bound. Not displayed if NULL (if !is.null(x)).
#' @param upper Numeric vector of the same length as x, containing the values to be displayed as an upper bound. Not displayed if NULL (if !is.null(x)).
#' @param weights Numeric vector of the same length as x, containing the relative length of dimensions. Values should be between 0 and 1.
#' @param main Character vector with one element containing the barplot's title. Defaults to NULL
#' @param max Numeric value determining the largest possible value of the data to be displayed. Defaults to 1.
#' @param xylim Numeric value determining both upper and lower limits on both x and y axis (xlim & ylim). Defaults to 1.5.
#' @details Plots a radar chart based on a numeric vector. Each dimension of the radar chart can be assigned a weight and/or an upper and lower bound.
#' @keywords plotting
#' @export
#' @examples
#' spiderplot()

spiderplot <- function (x = NULL, lower = NULL, upper = NULL, weights = NULL, 
    names = NULL, main = NULL, max = 1, xylim = 1.5, add.scale = T) 
{
    dimensions = x
    if (is.null(dimensions) & is.null(weights) & is.null(main)) {
        dimensions = c(0.1, 0.2, 0.3, 0.8, 0.7, 0.1, 0.1, 0.1, 
            0.1, 0.2)
        lower = c(0, 0, 0.2, 0.7, 0.6, 0, 0, 0, 0, 0)
        upper = c(0.2, 0.4, 0.4, 0.9, 0.8, 0.2, 0.2, 0.2, 0.1, 
            0.3)
        weights = c(1, 1, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5)
        main = "Consideration of option 1"
        names = c("artist's opinion", "restoration ethics", "historicity", 
            "authenticity", "functionality", "relative importance", 
            "legal aspects", "technical limit./poss.", "aest./art. factors", 
            "financial limit./poss.")
    }
    if (length(weights) == 1) 
        weights = rep(weights[1], length(dimensions))
    if (sum(dimensions < 0, na.rm = T) > 0) {
        warning("Values smaller than 0 are set to 0")
        dimensions[dimensions < 0] = 0
    }
    if (is.null(weights)) 
        weights = rep(1, length(dimensions))
    weights2 = c(weights, weights[1])
    if (!is.null(names)) 
        names(dimensions) = names
    theta = seq(0, 2 * pi, length = length(dimensions) + 1)
    plot(weights2 * cos(theta), weights2 * sin(theta), type = "l", 
        lty = 1, col = rgb(0, 0, 0), axes = F, xlab = "", ylab = "", 
        xlim = c(-xylim, xylim), ylim = c(-xylim, xylim))
    if (!is.null(upper) & !is.null(lower)) {
        polygon(weights2 * c(upper, upper[1])/max(upper, max) * 
            cos(theta), weights2 * c(upper, upper[1])/max(upper, 
            max) * sin(theta), col = rgb(0, 0, 1, 0.5), lty = 1)
        polygon(weights2 * c(lower, lower[1])/max(lower, max) * 
            cos(theta), weights2 * c(lower, lower[1])/max(lower, 
            max) * sin(theta), col = rgb(1, 1, 1), lty = 1)
    }
    lines(weights2 * 0.5 * cos(theta), weights2 * 0.5 * sin(theta), 
        lty = 2, col = rgb(0, 0, 0, 0.2))
    segments(0, 0, weights2 * cos(theta), weights2 * sin(theta), 
        lty = 1, col = rgb(0, 0, 0, 0.2))
    if (!is.null(dimensions)) 
        lines(weights2 * c(dimensions, dimensions[1])/max(dimensions, 
            max) * cos(theta), weights2 * c(dimensions, dimensions[1])/max(dimensions, 
            max) * sin(theta), col = "blue", lwd = 2)
    text(weights * cos(theta)[-(length(dimensions) + 1)], weights * 
        sin(theta)[-(length(dimensions) + 1)], paste0(1:length(dimensions), 
        ". ", names(dimensions)), pos = ifelse(cos(theta)[-(length(dimensions) + 
        1)] < 0, 2, 4), xpd = T, cex = 0.6)
    text(c(1 * weights2[1], 0.5 * weights2[1]), c(0, 0) + 0.05, 
        c(round(max(dimensions, max), 2), round(max(dimensions, 
            max)/2, 2)), cex = 0.6, pos = 2, col = "darkgrey")
    if (length(dimensions)/2 == round(length(dimensions)/2)) 
        text(c(-1 * weights2[round(length(weights2)/2)], -0.5 * 
            weights2[round(length(weights2)/2)]), c(0, 0) + 0.05, 
            c(round(max(dimensions, max), 2), round(max(dimensions, 
                max)/2, 2)), cex = 0.6, pos = 4, col = "darkgrey")
    title(main, cex.main = 0.8)
}
