#' Creates a SUS plot with acceptability criteria indicated
#'
#' \code{sus_suaro} creates a SUS plot as shown in Suaro's book.
#' Line segments and text can be added on top of the base plot.
#'
#' @param ... Optional parameters which can be supplied to
#'   \code{\link[ggplot2:geom_path]{geom_line}}.
#'
#' @return Returns a base plot of SUS with acceptability criteria.
#' Line segments and text can be added on top of the base plot
#'
#' @seealso \code{\link{sus_to_percentile}}, \code{\link{gg_sus}}
#'
#' @examples
#' #setwd("C:/Pictures")
#' # ppi <- 300
#' # png("SUS.png", height = 2.5*ppi, width = 8*ppi, res = ppi)
#' sus_suaro()
#' segments(x0 = 63.2, y0 = 0, x1 = 63.2, y1 = .6,
#'   col = "red", lty = 2)
#' text(x = 63.2, y = .63, adj = c(.5,0),
#'   label = "IOT = 63.2", col = "red", cex = .8)
#' # dev.off()
#' @export
sus_suaro <- function(){
  par(mai = c(.5,1.25,.5,0), mgp = c(1.25,.25,0))
  plot(x = c(0,105), y = c(0,1), type = "n", yaxt = "n", xaxt = "n",
    ylab = "", xlab = "SUS Score", bty = "n", xaxs = "i", yaxs = "i")
  axis(side = 1, seq(0,100,10), tck = .40)
  axis(side = 1, seq(5,95,10), labels = rep("", 10), tck = .20)
  abline(v = c(25, 39, 52, 73, 85, 100), lty = 3)
  polygon(x = c(0,0,100,100), y = c(.60,100,100,.65),
    col = "white", border = "white")

  text(x = c(25, 39, 52, 73, 85, 100), y = .67, cex = .7,
    label = c("WORST\nIMAGINABLE", "POOR", "OK",
              "GOOD", "EXCELLENT", "BEST\nIMAGINABLE"), adj = c(.5,0))

  polygon(x = c(0,0,50,50), y = c(1,.9,.9,1),
    angle = -45, density = 15, lwd = 2, border = "transparent")
  polygon(x = c(50,50,62.5,62.5), y = c(1,.9,.9,1),
    col = "grey50", border = "white")
  polygon(x = c(62.5,62.5,70,70), y = c(1,.9,.9,1),
    col = "grey50", border = "white")
  polygon(x = c(70,70,100,100), y = c(1,.9,.9,1),
    angle = 45, density = 15, lwd = 2, border = "transparent")

  text(x = mean(c(62.5,50)), y = .95,
    label = "LOW", col = "white", cex = .7, font = 2)
  text(x = mean(c(62.5,70)), y = .95,
    label = "HIGH", col = "white", cex = .7, font = 2)
  mtext(side = 3, at = c(25,60,85),
    text = c("NOT ACCEPTABLE","MARGINAL","ACCEPTABLE"),
    cex = .7, font = 2)

  mtext(side = 2, at = 1, text = "ACCEPTABILITY\nRANGES",
    las = 1, cex = .8, adj = .5, line = 2.75)
  mtext(side = 2, at = .7, text = "ADJECTIVE\nRATINGS",
  las = 1, cex = .8, adj = .5, line = 3.00)
}