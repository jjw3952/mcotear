#' Creates residual plots
#'
#' \code{residual_plots} creates a four-pack of plots similar to the
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
#' @seealso \link[stats]{plot.lm}, \link{gg_residual_plots},
#'   \link{df_from_model} 
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
#' residual_plots(npk.table)
#'
#' @export
residual_plots <- function(
  x, ...){

  res <- x[,1]
  fit <- x[,2]

  par(mfrow = c(2,2))
  {qqnorm(y = res, main = "Normal QQ Plot of Residuals", ...);
    qqline(y = res)} 
 
 {plot(x = fit, y = res, type = "n",
   main = "Residuals vs Fitted Values", ...); 
   abline(h = 0, lty = 2);
   points(x = fit, y = res, ...)}

  hist(x = res, main = "Histogram of the Residuals", ...)

  {plot(x = 1:length(res), y = res, type = "n", xlab = "Order",
    main = "Residual vs Order", ...);
    abline(h = 0, lty = 2);
    points(x = 1:length(res), y = res, type = "o", ...)}
}