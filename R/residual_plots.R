#' Creates a data.frame from a model
#'
#' \code{df_from_model} creates and returns a data.frame from a
#' model object such as a(n): \code{\link[stats]{aov}}, 
#' \code{\link[stats]{lm}}, or \code{\link[stats]{glm}} object.
#' The return data.frame will contain two columns, one for
#' residuals ('res'), and a second for fitted values ('fit').
#'
#' @param model A model object such as a(n): \code{\link[stats]{aov}}, 
#'   \code{\link[stats]{lm}}, or \code{\link[stats]{glm}} object, that
#'   is compatible with \code{\link[stats:fitted.values]{fitted}} and
#'   \code{\link[stats]{residuals}} methods.
#' @param type The type of residuals to be returned in the data.frame.
#'   The residual type supplied must match a function name, for example:
#'   \code{\link[stats]{residuals}}, \code{\link[stats:influence.measures]{rstandard}},
#'   and \code{\link[stats:influence.measures]{rstudent}}. Values can be supplied as quoted or
#'   unquoted elements, for example: "rstandard" or rstandard.
#'
#' @return The output will be a two-column \code{\link[base]{data.frame}}
#'   with columns 'res' and 'fit' containing the residuals and fitted values.
#'
#' @seealso  \link[stats]{plot.lm}, \link{residual_plots},
#'   \link{gg_residual_plots}
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
#' df_from_model(npk.aov1, type = rstandard)
#' df_from_model(npk.aov1)
#' 
#' df_from_model(npk.aov1, type = "rstudent")
#' df_from_model(npk.aov1, type = "residuals")
#' df_from_model(npk.aov1, type = "resid")
#' 
#' \dontrun{
#' df_from_model(npk.aov1, resid)
#' }
#' 
#' @export

df_from_model <- function(model, type = "rstandard"){

  res <- do.call(what = type, list(model))
  fit <- fitted(model) 

  return(data.frame(res, fit))
}



# Next function, create residual plots -----------------------------------------
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




# Next function, create residual plots -----------------------------------------
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


# End of File
#---------#---------#---------#---------#---------#---------#---------#---------