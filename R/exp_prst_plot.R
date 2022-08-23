#' Probability Ratio Sequential Test (PRST) Plot
#'
#' \code{exp_prst_plot} returns parameters defining a PRST for the exponential distribution
#'   (as developed by Abraham Wald), and creates the plot of the results and defined parameters.
#'
#' @param r (Optional, default as NULL) The cumulative number of failures up to time \code{t}.
#'   If specified it must be of equal length to \code{t}.
#' @param t (Optional, default as NULL) The cumulative (operational) time.
#'   If specified it must be of equal length to \code{r}.
#' @param mtbf0 The minimum acceptable MTBF.
#' @param mtbfa A value of MTBF greater than \code{mtbf0} at which we want to
#'   ensure we don't reject the system.
#' @param alpha The producer's risk of rejecting equipment with MTBF > MTBFA
#'   (the probility of rejecting good equipment). Default = 0.10.
#' @param beta The consumer's risk of accepting equipment with MTBF < MTBF0.
#'   (the probility of accepting bad equipment). Default = 0.10.
#'
#' @return The output will return \code{r} = the number of failures at which
#'   the test terminated, \code{T} = the time at which the test is terminated,
#'   \code{a} = y-intercept for the accept time, \code{b} = slope of the accept
#'   and reject lines, \code{c} = y-intercept of the reject line, \code{df} =
#'   a \code{data.frame} giving the number of failures and the associated time
#'   times at which the test is terminated and the equipment accepted or rejected,
#'   as well as some text which outputs the form of the accept/reject lines, and
#'   the accept/reject times. A plot will also be returned plotting the accept/reject
#'   lines, truncation lines, and the observed data as step function if supplied by
#'   parameters \code{r} and \code{t}.
#'
#' @seealso \code{\link{exp_reliability_req}}, \code{\link{exp_test_duration}},
#'   \code{\link{exp_test_demo}}, \code{\link{exp_prst}}
#'
#' @references
#' Mil-Hdbk-781A
#' 
#' Wald, Abraham. Sequential Analysis. John Wiley & Sons, 1947.
#' 
#' Brazovsky, Igor. Reliability Theory and Practice. Prentice Hall, 1961.
#'
#' @examples
#' exp_prst_plot(r=NULL, t=NULL, mtbf0=100, mtbfa=200, alpha=.1, beta=.1)
#' exp_prst_plot(
#'   r = c(0,1,2,3,4,5,6,6,14),
#'   t = c(0,100,150,200,250,275,300,1500,2060),
#'   mtbf0=100, mtbfa=200, alpha=.1, beta=.1)
#'
#' @export
exp_prst_plot <- function(r=NULL, t=NULL, mtbf0=100, mtbfa=200, alpha=.1, beta=.1){

  if(mtbf0 >= mtbfa){
    stop("mtbfa must be > mtbf0")
  }
  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0){
    stop("alpha and beta must both be between 0 and 1")
  }

  fail_dat <- data.frame(r, t)

  prst_calcs <- exp_prst(mtbf0, mtbfa, alpha, beta)
  
  regions <- with(prst_calcs, data.frame(
    x = c(0, (r-c)/b, T, -a/b, T, T),
    y = c(c, r, r, 0, a+(b*T), r),
    Decision = rep(c("Reject", "Accept"), each = 3)
  ))
  
  x <- with(prst_calcs, seq(-a/b, T, length.out = 100))
  accept <- with(prst_calcs, a+b*x)
  df_accept_ribbon <- data.frame(x = x, accept = accept)

  x <- with(prst_calcs, seq(0, (r-c)/b, length.out = 100))
  reject <- with(prst_calcs, c+b*x)
  df_reject_ribbon <- data.frame(x = x, reject = reject)

  fail_col <- "red2"
  pass_col <- "green2"
  fail_dat <- merge(fail_dat, prst_calcs$df, all.x = TRUE)
  fail_dat$color <- ifelse(fail_dat$t > fail_dat$accept_le_r, pass_col,
      ifelse(fail_dat$t < fail_dat$reject_ge_r, fail_col, "black")) 
  fail_dat$color[is.na(fail_dat$color)] <- "black"
  fail_dat$color[fail_dat$r >= prst_calcs$r] <- fail_col
  fail_dat$color[fail_dat$r < prst_calcs$r & fail_dat$t >= prst_calcs$T] <- pass_col

  prst_plot <- ggplot(regions) +
  geom_line(aes(x = x, y = y, colour = Decision, group = Decision), size = 1.5) +
  geom_abline(aes(intercept = 0, slope = prst_calcs$b), linetype = "dashed") +
  scale_colour_manual("Decision", values = c("Accept" = "green2", "Reject" = "red")) +
  scale_y_continuous(breaks = seq(0, 2*prst_calcs$r)) +
  labs(x = "Time", y = "Failures",
    subtitle = paste0("Truncation Time = ", prst_calcs$T,
               "\nTruncation Failures = ", prst_calcs$r)
  ) +
  theme(panel.grid.minor.y = element_blank()) +
  geom_ribbon(
    data = df_accept_ribbon,
    mapping = aes(x = x, ymin = 0, ymax = accept),
    fill = "green2", alpha = .2) +
  geom_ribbon(
    data = df_reject_ribbon,
    mapping = aes(x = x, ymin = reject, ymax = prst_calcs$r),
    fill = "red", alpha = .2) +
  geom_ribbon(
    data = df_reject_ribbon,
    mapping = aes(x = x, ymin = reject, ymax = prst_calcs$r),
    fill = "red", alpha = .2) +
  geom_step(data = fail_dat, aes(x = t, y = r)) +
  geom_point(data = fail_dat, aes(x = t, y = r, fill = I(color)), shape = 21)

  #print(prst_plot)
  return(list(prst_calcs, plot = prst_plot))
}