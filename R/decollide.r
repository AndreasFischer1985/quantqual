#' Function decollide
#' 
#' Returns a matrix with coordinates and text for plotting texts without collisions.
#' @param x Numeric vector containing x-coordinates of text elements.
#' @param y Numeric vector containing x-coordinates of text elements.
#' @param text Character vector containing text-elements to be plotted.
#' @param jitter Logical value specifying whether to add a small random component after each step.
#' @param cex Numeric value specifying the relative font size of the text. Defaults to 1.
#' @param frame Numeric value specifying the relative font size of an invisible frame around the text. Defaults to 1.5
#' @param lock.x Logical value specifying whether to skip shifts on the x-axis. Defaults to F.
#' @param lock.y Logical value specifying whether to skip shifts on the y-axis. Defaults to F.
#' @param verbose Logical value specifying whether to give information on the process after each step.
#' @param cex Numeric value specifying the maximum number of steps. Defaults to 100.
#' @details Returns a matrix with coordinates and text for plotting texts without collisions. Iteratively shifts text elements until all collisions are eliminated or the maximum number of repetitions is reached.
#' @keywords plotting
#' @export
#' @examples
#' set.seed(0);decollide(x=rnorm(10),y=rnorm(10),text=paste0(1:10,":",round(rnorm(10),3)),cex=1,font=1,plot=T)

decollide <- function (x, y, text, jitter = F, cex = 1, font = 1, frame = 1.5, 
    plot = F, lock.x = F, lock.y = F, verbose = F, repetitions = 100) 
{
    x0 = x
    y0 = y
    if (plot) {
        xrange = max(x) - min(x)
        yrange = max(y) - min(y)
        plot(x, y, type = "n", xlim = c(min(x) - xrange/2, max(x) + 
            xrange/2), ylim = c(min(y) - yrange/2, max(y) + yrange/2))
        text(x, y, text, font = font, cex = cex)
    }
    else xrange = par("usr")[2] - par("usr")[1]
    yrange = par("usr")[4] - par("usr")[3]
    sw = strwidth(text, font = font, cex = cex * frame, units = "user")
    sh = strheight(text, font = font, cex = cex * frame, units = "user")
    co = cbind(x - sw/2, x + sw/2, y - sh/2, y + sh/2)
    m = matrix(0, nrow = dim(co)[1], ncol = dim(co)[1])
    for (i in 1:repetitions) {
        for (i in 1:dim(co)[1]) for (j in 1:dim(co)[1]) if (i != 
            j) {
            col = co[j, 1] <= co[i, 2] & co[j, 2] >= co[i, 1] & 
                ((co[j, 3] <= co[i, 4] & co[j, 4] >= co[i, 3]) | 
                  (co[j, 4] >= co[i, 3] & co[j, 3] <= co[i, 4]))
            m[i, j] = col
            if (col) {
                if (x[i] >= x[j] & !lock.x) 
                  x[i] = x[i] + xrange/100
                if (x[i] < x[j] & !lock.x) 
                  x[i] = x[i] - xrange/100
                if (y[i] >= y[j] & !lock.y) 
                  y[i] = y[i] + yrange/100
                if (y[i] < y[j] & !lock.x) 
                  y[i] = y[i] - yrange/100
                if (jitter) {
                  co = cbind(x - sw/2, x + sw/2, y - sh/2, y + 
                    sh/2)
                  col = co[j, 1] < co[i, 2] & co[j, 2] > co[i, 
                    1] & ((co[j, 3] < co[i, 4] & co[j, 4] > co[i, 
                    3]) | (co[j, 4] > co[i, 3] & co[j, 3] < co[i, 
                    4]))
                  if (col) {
                    if (abs(x[i] - x[j]) >= 0 & !lock.x) 
                      x[i] = x[i] + ifelse(jitter, rnorm(1, sd = sd(x)/100), 
                        0)
                    if (abs(y[i] - y[j]) >= 0 & !lock.y) 
                      y[i] = y[i] + ifelse(jitter, rnorm(1, sd = sd(y)/100), 
                        0)
                  }
                }
            }
            co = cbind(x - sw/2, x + sw/2, y - sh/2, y + sh/2)
        }
        if (verbose) 
            message(sum(m)/2)
        if (plot) {
            plot(x, y, type = "n", xlim = c(min(x) - xrange/2, 
                max(x) + xrange/2), ylim = c(min(y) - yrange/2, 
                max(y) + yrange/2))
            text(x, y, text, font = font, cex = cex)
        }
        if (sum(m)/2 == 0) 
            break
    }
    return(data.frame(x, y, text))
}
