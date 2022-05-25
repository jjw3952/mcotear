#' Creates residual plots in ggplot2
#'
#' \code{gg_residual_plots} creates a four-pack of plots similar to the
#' residual plots returned by Minitab. Plots include (1) a Normal QQ plot of
#' the residuals, (2) a residuals vs fitted values plot, (3) a histogram 
#' of the residuals, and (4) a plot of residuals vs order.
#' The 4th plot (residuals vs order),
#' assumes the data.frame supplied is already in order.
#'
#' @param x A data.frame object (such as that returned from
#'   \code{\link{df_from_model}}) that contains two columns, the first
#'   of which is the residuals, and the second is fitted values.
#'
#' @return Residual plot(s) useful for checking model assumptions.
#'
#' @seealso \link[stats]{plot.lm}, \link{residual_plots}, \link{df_from_model} 
#'
#' @examples
#' data(npk)
#' npk.aov <- aov(yield ~ block + N*P*K, npk)
#' summary(npk.aov)
#' npk.aov1 <- aov(yield ~ block + N + K, data = npk)
#' summary(npk.aov1)
#' summary.lm(npk.aov1)
#' model.tables(npk.aov1, type = "means", se = TRUE)
#' plot(npk.aov1)
#'
#' npk.table <- df_from_model(npk.aov1, type = "rstandard")
#' gg_residual_plots(npk.table)
#'
#' @export
gg_residual_plots <- function(
  x, ...){

  p <- ggplot2::ggplot(x)

  # following four lines from base R's qqline()
  y <- quantile(x[,1], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  p1 <- p + ggplot2::stat_qq(ggplot2::aes(sample = res), ...) +
    ggplot2::geom_abline(slope = slope, intercept = int) +
    ggplot2::ggtitle("Normal QQ Plot of Residuals")

  p2 <- p + ggplot2::geom_point(ggplot2::aes(x = fit, y = res), ...) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::ggtitle("Residuals vs Fitted Values")

  p3 <- p + ggplot2::geom_histogram(ggplot2::aes(x = res), ...) +
    ggplot2::ggtitle("Histogram of the Residuals")

  p4 <- p + ggplot2::geom_point(ggplot2::aes(x = 1:length(res), y = res), ...) +
    ggplot2::geom_line(ggplot2::aes(x = 1:length(res), y = res), ...) +
    ggplot2::ggtitle("Residual vs Order") +
    labs(x = "Order")

  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}