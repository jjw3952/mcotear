#' Converts a System Usability Scale score to a corresponding percentile rank
#'
#' \code{sus_to_percentile} converts a valid SUS score (0:100), into
#'   the corresponding percentile rank (0.00:1.00).
#'
#' @param x A valid SUS score ranging within the range 0:100.
#'
#' @return Returns a \code{\link[base]{data.frame}} of x- (SUS scores),
#'   and y-values (percentiles).
#'
#' @seealso \code{\link{gg_sus}}, \code{\link{sus_suaro}}
#'
#' @examples
#' sus_to_percentile(c(68, 90))
#' @export

sus_to_percentile <- function(x){
  df <- structure(
    list(
      score = c(0L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 
        40L, 45L, 50L, 55L, 60L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 
        73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L, 85L, 90L, 95L, 100L),
 
      percentile = c(0, 0.003, 0.004, 0.007, 0.01, 0.015, 0.02, 
        0.04, 0.06, 0.08, 0.13, 0.19, 0.29, 0.41, 0.44, 0.47, 0.5, 
        0.53, 0.56, 0.6, 0.63, 0.67, 0.7, 0.73, 0.77, 0.8, 0.83, 
        0.86, 0.88, 0.97, 0.998, 0.99999, 1)),

     .Names = c("score", "percentile"),
     class = "data.frame", row.names = c(NA, -33L))

  model <- smooth.spline(
    x = df$score,
    y = df$percentile,
    spar = 100^-1000, all.knots = T, tol = 10^-1)

  return( data.frame(predict(model, x)))
}





#' Creates a SUS curve plot
#'
#' \code{gg_sus} creates a SUS plot in ggplot2 graphics.
#'
#' @param ... Optional parameters which can be supplied to
#'   \code{\link[ggplot2:geom_path]{geom_line}}.
#'
#' @return Returns ggplot2 plot of SUS scores vs percentile ranks.
#'
#' @seealso \code{\link{sus_to_percentile}}, \code{\link{sus_suaro}}
#'
#' @examples
#' p <- gg_sus()
#' p
#'
#' df <- sus_to_percentile(68)
#' p + geom_point(data = df, aes(x, y), colour = "red") 
#'
#' df <- cbind(df, Phase = "End of Record Test") 
#' p + 
#'   geom_point(data = df, aes(x, y, colour = Phase)) +
#'   geom_segment(data = df, aes(x = 0, xend = x, y = y, yend = y,
#'     colour = Phase, linetype = Phase)) +
#'   geom_segment(data = df, aes(x = x, xend = x, y = 0, yend = y,
#'     colour = Phase, linetype = Phase)) +
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0)) +
#'   scale_colour_manual(breaks = "End of Record Test", values = "#E69F00") +
#'   scale_linetype_manual(breaks = "End of Record Test", values = 2) +
#'   labs(y = "Percentile Rank", x = "SUS Score")
#' @export

gg_sus <- function(...){
  df <- data.frame(sus_to_percentile())

  ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
}






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