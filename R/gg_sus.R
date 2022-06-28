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
#' 
#' rm(list = c("p", "df"))
#'
#' @export

gg_sus <- function(...){
  df <- data.frame(sus_to_percentile())

  ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
}
