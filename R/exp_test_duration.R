#' Calculates the required test length to meet a MTBF (Reliability) requirement
#'
#' \code{exp_test_duration} calculates the required test duration (hours, miles,
#'   rounds, etc.) needed to meet a Mean Time Between Failure (Reliability)
#'   requirement given an allowable number of failures, a given level of
#'   confidence, and under the assumptions of exponentially distributed times
#'   between failures, and a time terminated test.
#'
#' @param r A numeric vector indicating the allowable number of failures.
#' @param mtbf A numeric Mean Time Between Failure requirement.
#'   This could be time, miles, rounds, etc.
#' @param alpha The allowable type I failure rate (1-confidence).
#'   Values must be within 0.0:1.00 default is set to 0.20.
#'
#' @return The output will be a three column \code{data.frame}
#'   with one column as the allowable number of failures,
#'   the second column defining the number of failures at which you reject,
#'   and the third column defining the required test
#'   duration (in the same units as the \code{mtbf} parameter), needed
#'   to satisfy the MTBF requirement with the given level of confidence,
#'   and the allowable number of failures.
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{exp_fixed_duration_tests}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @examples
#' # What is the required test duration to demonstrate the MTBF is at least
#'   # 228 hours, given 0:5 failures, and 80% confidence?
#' exp_test_duration(r = 0:5, mtbf = 228, alpha = 0.20)
#'
#' @export
exp_test_duration <- function(r, mtbf, alpha = 0.20){

  if(alpha >= 1 | alpha <= 0){
    stop("alpha must be between 0 and 1")
  }

  chisq <- qchisq(p = alpha, df = 2*(r+1), lower.tail = FALSE)
  duration <- mtbf*chisq/2
  return( data.frame(Accept = r, Reject = r+1, Duration = duration) )
}