#' Function spiderplot
#' 
#' Plots a radar chart based on a numeric vector.
#' @param x Numeric vector containing the values to be displayed. Values should be between 0 and 1.
#' @param lower Numeric vector of the same length as x, containing the values to be displayed as an lower bound. Not displayed if NULL (if !is.null(x)).
#' @param upper Numeric vector of the same length as x, containing the values to be displayed as an upper bound. Not displayed if NULL (if !is.null(x)).
#' @param weights Numeric vector of the same length as x, containing the relative length of dimensions. Values should be between 0 and 1.
#' @param main Character vector with one element containing the barplot's title. Defaults to NULL
#' @param max Numeric value determining the largest possible value of the data to be displayed. Defaults to 1.
#' @param min Numeric value determining the smalles possible value of the data to be displayed. Defaults to 0.
#' @param xylim Numeric value determining both upper and lower limits on both x and y axis (xlim & ylim). Defaults to 1.5.
#' @param col Character or rgb value specifying the line's color. Defaults to rgb(0,0,1).
#' @param col2 Character or rgb value specifying the color of the area between upper and lower bound. Defaults to rgb(0,0,1,.5).
#' @param border Character or rgb value specifying the color of the polygon border. Defaults to NA.
#' @param mode Numeric value specifying whether to draw a polygon (mode=1) or arrows (mode!=1), and whether to draw a polygon for x (mode>=0) or not (mode<0). Defaults to 0.
#' @param arrows.lwd Numeric value specifying lwd for arrows (if any). Defaults to 2.
#' @param arrows.length Numeric value specifying lwd for arrows (if any). Defaults to .01.
#' @param add.numbers Logical value specifying whether to add numbers to labels. Defaults to F.
#' @param grid.numbers Numeric value specifying the number of decimal values shown at the grid. Defaults to 2.
#' @param add.grid Logical value specifying whether to add numbers to add grid lines. Defaults to T.
#' @param add.labels Logical value specifying whether to add labels to the plot. Defaults to T.
#' @details Plots a radar chart based on a numeric vector. Each dimension of the radar chart can be assigned a weight and/or an upper and lower bound.
#' @keywords plotting
#' @export
#' @examples
#' spiderplot()

spiderplot <- function (x = NULL, lower = NULL, upper = NULL, weights = NULL, 
    names = NULL, main = NULL, max = 1, min = 0, xylim = 1.5, 
    add.scale = T, col = "#00547A", col2 = "#FFA500E6", border = NA, 
    mode = 0, arrows.lwd = 3, arrows.length = 0.01, add.numbers = F, 
    numbers = 2, add.grid = T, add.labels = T) 
{
    dimensions = x
    if (!is.null(upper) & !is.null(dimensions)) {
        if (max < max(c(lower, upper, dimensions))) 
            max = max(c(lower, upper, dimensions))
        if (min > min(c(lower, upper, dimensions))) 
            min = min(c(lower, upper, dimensions))
    }
    else {
        if (!is.null(dimensions)) {
            if (max < max(dimensions)) 
                max = max(dimensions)
            if (min > min(dimensions)) 
                min = min(dimensions)
        }
        if (!is.null(upper)) {
            if (max < max(upper)) 
                max = max(upper)
            if (min > min(upper)) 
                min = min(upper)
        }
        if (!is.null(lower)) {
            if (max < max(lower)) 
                max = max(lower)
            if (min > min(lower)) 
                min = min(lower)
        }
    }
    if (is.null(dimensions) & is.null(weights) & is.null(main)) {
        dimensions = c(0.1, 0.2, 0.3, 0.8, 0.7, 0.1, 0.1, 0.1, 
            0.1, 0.2)
        x = dimensions
        lower = c(0, 0, 0.2, 0.7, 0.6, 0, 0, 0, 0, 0)
        upper = c(0.2, 0.4, 0.4, 0.9, 0.8, 0.2, 0.2, 0.2, 0.1, 
            0.3)
        weights = 1
        main = "Consideration of option 1"
        names = c("artist's opinion", "restoration ethics", "historicity", 
            "authenticity", "functionality", "relative importance", 
            "legal aspects", "technical limit./poss.", "aest./art. factors", 
            "financial limit./poss.")
    }
    if (length(weights) == 1) 
        weights = rep(weights[1], length(dimensions))
    if (is.null(weights)) 
        weights = rep(1, length(dimensions))
    weights2 = c(weights, weights[1])
    if (is.null(names(dimensions))) 
        names(dimensions) = 1:length(dimensions)
    if (!is.null(names)) 
        names(dimensions) = names
    theta = seq(0, 2 * pi, length = length(dimensions) + 1)
    plot(weights2 * cos(theta), weights2 * sin(theta), type = "l", 
        lty = 1, col = rgb(0, 0, 0), axes = F, xlab = "", ylab = "", 
        xlim = c(-xylim, xylim), ylim = c(-xylim, xylim))
    no = function(x) {
        (x - min(c(min, x)))/(max(c(max, max(x))) - min(min, 
            min(x)))
    }
    if (!is.null(upper) & !is.null(lower)) {
        if (mode == 1) {
            polygon(weights2 * no(c(upper, upper[1])) * cos(theta), 
                weights2 * no(c(upper, upper[1])) * sin(theta), 
                col = col2, border = border, lty = 1)
            polygon(weights2 * no(c(lower, lower[1])) * cos(theta), 
                weights2 * no(c(lower, lower[1])) * sin(theta), 
                col = rgb(1, 1, 1), border = border, lty = 1)
        }
        else {
            arrows((weights2 * no(c(lower, lower[1])) * cos(theta))[-length(weights2)], 
                (weights2 * no(c(lower, lower[1])) * sin(theta))[-length(weights2)], 
                (weights2 * no(c(upper, upper[1])) * cos(theta))[-length(weights2)], 
                (weights2 * no(c(upper, upper[1])) * sin(theta))[-length(weights2)], 
                col = col2, angle = 90, length = arrows.length, 
                code = 3, lwd = arrows.lwd)
        }
    }
    if (add.grid) {
        lines(weights2 * 0.5 * cos(theta), weights2 * 0.5 * sin(theta), 
            lty = 2, col = rgb(0, 0, 0, 0.2))
        segments(0, 0, weights2 * cos(theta), weights2 * sin(theta), 
            lty = 1, col = rgb(0, 0, 0, 0.2))
    }
    if (!is.null(dimensions) & mode >= 0) 
        lines(weights2 * no(c(dimensions, dimensions[1])) * cos(theta), 
            weights2 * no(c(dimensions, dimensions[1])) * sin(theta), 
            col = col, lwd = 2)
    if (add.grid & numbers > 0) 
        if (!is.null(upper) & !is.null(lower)) {
            text(c(1 * weights2[1], 0.5 * weights2[1]), c(0, 
                0) + 0.05, c(round(max, numbers), round(((max + 
                min)/2), numbers)), cex = 0.6, pos = 2, col = "darkgrey")
            if (length(dimensions)/2 == round(length(dimensions)/2)) 
                text(c(-1 * weights2[round(length(weights2)/2)], 
                  -0.5 * weights2[round(length(weights2)/2)]), 
                  c(0, 0) + 0.05, c(round(max, numbers), round(((max + 
                    min)/2), numbers)), cex = 0.6, pos = 4, col = "darkgrey")
        }
        else {
            text(c(1 * weights2[1], 0.5 * weights2[1]), c(0, 
                0) + 0.05, c(round(max, numbers), round(((max + 
                min)/2), numbers)), cex = 0.6, pos = 2, col = "darkgrey")
            if (length(dimensions)/2 == round(length(dimensions)/2)) 
                text(c(-1 * weights2[round(length(weights2)/2)], 
                  -0.5 * weights2[round(length(weights2)/2)]), 
                  c(0, 0) + 0.05, c(round(max, numbers), round(((max + 
                    min)/2), numbers)), cex = 0.6, pos = 4, col = "darkgrey")
        }
    if (add.labels) 
        if (add.numbers) {
            text(weights * cos(theta)[-(length(dimensions) + 
                1)], weights * sin(theta)[-(length(dimensions) + 
                1)], paste0(1:length(dimensions), ". ", names(dimensions)), 
                pos = ifelse(cos(theta)[-(length(dimensions) + 
                  1)] < 0, 2, 4), xpd = T, cex = 0.6)
        }
        else {
            text(weights * cos(theta)[-(length(dimensions) + 
                1)], weights * sin(theta)[-(length(dimensions) + 
                1)], paste0(names(dimensions)), pos = ifelse(cos(theta)[-(length(dimensions) + 
                1)] < 0, 2, 4), xpd = T, cex = 0.6)
        }
    title(main, cex.main = 0.8)
}
