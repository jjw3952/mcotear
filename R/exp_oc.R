#' Exponential Operating Characteristic (OC) Curves
#'
#' \code{exp_oc} computes OC curves for exponential reliability tests
#'   (probability of passing test as a function of true MTBF). See pg. 67
#'   for the OC formula in MIL-HDBK-781A.
#'
#' @param accept A vector with the allowable number of failures.
#' @param duration A vector of test durations.
#' @param mtbf A vector of the true MTBFs.
#'
#' @return The probability of passing the test of a given duration with an
#'   allowable number of failures, given the true MTBF.
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{exp_fixed_duration_tests}}
#'
#' @examples
#'   # Theta is MTBF, 0 is the number at which you accept - system passes test
#'   # 804.719 is the test duration
#'   theta <- seq(250, 5000, 50)
#'   prob_pass <- exp_oc(accept = 0, duration = 804.719, mtbf = theta)
#'   prob_pass[1:5]
#'
#'   ggplot2::ggplot(data.frame(prob_pass = prob_pass, theta = theta)) +
#'     ggplot2::geom_line(ggplot2::aes(x = theta, y = prob_pass)) +
#'     ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
#'
#'   ggplot2::ggplot(data.frame(x=c(250,5000))) +
#'     ggplot2::stat_function(aes(x=x), fun = exp_oc, args = list(accept = 0, duration = 804.719)) +
#'     ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'     ggplot2::labs(x = "MTBF", y = "Prob of Passing Demonstration")
#'
#' @references
#' Mil-Hdbk-781A
#'
#' @export
exp_oc <- function(accept, duration, mtbf){
  prob_pass <- ppois(accept, duration/mtbf, lower.tail=TRUE)
  prob_pass
}
