#' Calculates 2-sided CI for an exponential mean
#'
#' \code{exp_mean_ci} calculates the 2-sided CI
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), number of failures, confidence level, and assuming
#'   the test is time terminated.
#'
#' @param r A numeric vector indicating the observed number of failures.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param alpha The allowable type I failure rate (1-confidence).
#'   Values must be within 0.0:1.00 default is set to 0.20.
#'
#' @return The output will be a numeric vector, indicating the LCB and UCB of
#'   the exponential mean with the given level of confidence. The units
#'   will be the same as the units of the supplied \code{duration}. 
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_equal_mtbf}},
#'   \code{\link{exp_fixed_duration_tests}}
#'
#' @examples
#' # What is the 80% 2-sided CI for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 367 hours, and 0 failures.
#' exp_mean_ci(r = 0, duration = 367, alpha = 0.20)
#'
#' # What is the 80% 2-sided CI for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 920 hours, and 7 failures.
#' exp_mean_ci(r = 7, duration = 920, alpha = 0.20)
#'
#' @export
exp_mean_ci <- function(r, duration, alpha = 0.20){

  if( alpha >= 1 | alpha <= 0){
    stop("alpha must be between 0 and 1")
  }

  lwr_p <- alpha/2
  chisq_lwr <- qchisq(p = lwr_p, df = 2 * (n + 1), lower.tail = FALSE)
  chisq_upr <- qchisq(p = lwr_p, df = 2 * n, lower.tail = TRUE)
  lwr <- 2 * duration/chisq_lwr
  upr <- 2 * duration/chisq_upr
  ci <- c(lwr, upr)
  names(ci) <- c(lwr_p, 1-lwr_p)
  return(ci)
}