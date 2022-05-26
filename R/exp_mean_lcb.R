#' Calculates the lower confidence bound for an exponential mean
#'
#' \code{exp_mean_lcb} calculates the lower confidence bound (LCB)
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), number of failures, confidence level, and assuming
#'   the test is time terminated.
#'
#' @param n A numeric vector indicating the observed number of failures.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a numeric vector, indicating the LCB of
#'   the exponential mean with the given level of confidence. The units
#'   will be the same as the units of the supplied \code{duration}. 
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_equal_mtbf}}
#'   \code{\link{exp_fixed_duration_tests}}
#'
#' @examples
#' # What is the 80% LCB for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 367 hours, and 0 failures.
#' exp_mean_lcb(n = 0, duration = 367, conf = 0.80)
#'
#' @export
exp_mean_lcb <- function(n, duration, conf = 0.80){

  if(conf >= 1 | conf <= 0){
    stop("conf must be between 0 and 1")
  }

  chisq <- qchisq(p = conf, df = 2*n+2, lower.tail = TRUE)
  2*duration/chisq
}