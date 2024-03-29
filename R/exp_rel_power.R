#' Power of a Reliability Test from Null and Alternative Hypotheses, alpha, and allowable failures
#'
#' \code{exp_rel_power} calculates the power of a reliability test given null and alternative
#'   hypotheses for MTTF/MTBF, the allowable numbers, the alpha error rate, and assuming
#'   the times are exponentially distributed. Hypothesis should be of the form,
#'   h0: MTBF <= X; h1: MTBF > X+Y, where X and Y are both positive values.
#'
#' @param mtbf0 A numeric value indicating the null hypothesis for MTBF/MTTR.
#' @param mtbfa A numeric value indicating the alternative hypothesis for MTBF/MTTR.
#'   (hours, miles, rounds, etc.).
#' @param r The allowable number of failures.
#' @param alpha The allowable type I failure rate (1-confidence).
#'   Values must be within 0.0:1.00 default is set to 0.20.
#'   Producer's risk - the probability we reject a system with MTBF = mtbfa.
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
#' exp_test_duration(r = 1, mtbf = 180, alpha = 0.2)
#' exp_rel_power(mtbf0 = 180, mtbfa = 300, r = 1, alpha = 0.2)
#'
#' mtbfa <- seq(100, 800, 1)
#' df <- data.frame(
#'   r = rep(c(1:5, 11), each = length(mtbfa)),
#'   mtbfa = mtbfa
#' )
#' df$power <- exp_rel_power(mtbf0 = 180, mtbfa = df$mtbfa, r = df$r, alpha = 0.2)
#' df$T <- exp_test_duration(r = df$r, mtbf = 180, alpha = 0.2)$Duration
#' df$label <- paste0("r = ", df$r, "; T = ", ceiling(df$T))
#' df$label <- factor(df$label, levels = unique(df$label))
#' 
#' ggplot(df) + 
#'   geom_path(
#'     aes(x = mtbfa, y = power, colour = label)
#'   ) +
#'   scale_colour_manual("", values = cbbPalette) +
#'   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   labs(
#'    y = "Probability of Passing Demonstration\n(Power)",
#'    x = "True MTBOMF",
#'    caption = "r = Allowable Failures\nT = Total Test Time Required (hours)\n\nConfidence set to 80%\nMTBF0 = 180 hours"
#'   ) +
#'   theme(plot.caption = element_text(hjust = 0))
#'
#' power <- .8
#' (sol <- uniroot(function(x) exp_rel_power(mtbf0=180,x,r=1,alpha=.2)-power, lower = 100, upper = 10000))
#' sol$root
#' sol$root/180
#' 
#' (sol <- uniroot(function(x) exp_rel_power(mtbf0=180,x,r=11,alpha=.2)-power, lower = 100, upper = 10000))
#' sol$root
#' sol$root/180
#'
#'
#' # At what value of MTBF would an "r" failure test have 80% power
#' # if we want to demonstrate MTBF > 500 with 80% confidence
#' r <- 0
#' power <- .8
#' mtbf0 <- 500
#' alpha <- .2
#' uniroot(function(x) exp_rel_power(mtbf0,x,r,alpha)-power, lower = 100, upper = 10000)
#' exp_test_duration(r, mtbf0, alpha)
#' 
#' # At what value of MTBF would an "r" failure test have 80% power
#' # if we want to demonstrate MTBF > 500 with 80% confidence
#' r <- 0:6
#' power <- .8
#' mtbfa <- NULL
#' for(i in seq_along(r)){
#'   mtbfa[i] <- uniroot(function(x) exp_rel_power(mtbf0,x,r[i],alpha)-power, lower = 100, upper = 10000)$root
#' }
#' mtbfa
#' exp_test_duration(r, mtbf0, alpha)
#' 
#' rm(list = c("r", "power", "mtbfa", "i", "mtbf0", "alpha", "sol", "df"))
#'
#' @export
exp_rel_power <- function(mtbf0, mtbfa, r, alpha = 0.20){
  pchisq(
    (mtbf0/mtbfa) * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1), lower.tail = FALSE
  )
}