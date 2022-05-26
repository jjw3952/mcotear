#' Power of a Reliability Test from Null and Alternative Hypotheses, alpha, and allowable failures
#'
#' \code{exp_rel_power} calculates the power of a reliability test given null and alternative
#'   hypotheses for MTTF/MTBF, the allowable numbers, the alpha error rate, and assuming
#'   the times are exponentially distributed. Hypothesis should be of the form,
#'   h0: MTBF <= X; h1: MTBF > X+Y, where X and Y are both positive values.
#'
#' @param mtbf0 A numeric value indicating the null hypothesis for MTBF/MTTR.
#' @param mtbf1 A numeric value indicating the alternative hypothesis for MTBF/MTTR.
#'   (hours, miles, rounds, etc.).
#' @param r The allowable number of failures.
#' @param alpha The desired level of significance (type 1 error rate) (values
#'   within 0.0:1.00, default is set to 0.20) (producer's risk - the probability we reject
#'   a system with MTBF = mtbf1).
#'
#' @return The output is a numeric value representing the power of rejecting
#'   the null hypothesis. 
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'   \code{\link{exp_fixed_duration_tests}}
#'
#' @examples
#' # Assuming the true MTBF is 300, what is the power of test  
#'   # to demonstrate MTBF > 180, with 80% power and confidence, assuming 
#'   # the true MTBF is 300 (and assuming the times between failure are
#'   # exponentially distributed)
#' exp_rel_power(mtbf0 = 180, mtbf1 = 300, r = 1, alpha = 0.2)
#'
#' mtbf1 <- seq(100, 600, 1)
#' df <- data.frame(
#'   r = rep(c(1:5, 11), each = length(mtbf1)),
#'   mtbf1 = mtbf1
#' )
#' df$power <- exp_rel_power(mtbf0 = 180, mtbf1 = df$mtbf1, r = df$r, alpha = .2)
#' df$T <- exp_test_duration(r = df$r, mtbf = 180, conf = .8)$Duration
#' df$label <- paste0("r = ", df$r, "; T = ", ceiling(df$T))
#' 
#' ggplot2::ggplot(df) + 
#'   ggplot2::geom_path(
#'     ggplot2::aes(x = mtbf1, y = power, colour = label)
#'   ) +
#'   ggplot2::scale_colour_manual("", values = cbbPalette) +
#'   ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   ggplot2::labs(
#'    y = "Probability of Passing Demonstration\n(Power)",
#'    x = "True MTBOMF",
#'    caption = "r = Allowable Failures\nT = Total Test Time Required (hours)\n\nConfidence set to 80%\nMTBF0 = 180 hours"
#'   ) +
#'   ggplot2::theme(plot.caption = element_text(hjust = 0))
#'
#' @export
exp_rel_power <- function(mtbf0, mtbf1, r = 1, alpha = 0.20){
  pchisq(
    (mtbf0/mtbf1) * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1), lower.tail = FALSE
  )
}