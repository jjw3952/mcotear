#' Calculates the required test length to meet a MTBF (Reliability) requirement
#'
#' \code{test_duration} calculates the required test duration (hours, miles,
#'   rounds, etc.) needed to meet a Mean Time Between Failure (Reliability)
#'   requirement given an allowable number of failures, a given level of
#'   confidence, and under the assumptions of exponentially distributed times
#'   between failures, and a time terminated test.
#'
#' @param r A numeric vector indicating the allowable number of failures.
#' @param mtbf A numeric Mean Time Between Failure requirement.
#'   This could be time, miles, rounds, etc.
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a two column \code{data.frame}
#'   with one column as the allowable number of failures, 
#'   and the second column defining the required test
#'   duration (in the same units as the \code{mtbf} parameter), needed
#'   to satisfy the MTBF requirement with the given level of confidence,
#'   and the allowable number of failures.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @examples
#' # What is the required test duration to demonstrate the MTBF is at least
#'   # 228 hours, given 0:5 failures, and 80% confidence?
#' test_duration(r = 0:5, mtbf = 228, conf = 0.80)
#'
#' @export
test_duration <- function(r, mtbf, conf = 0.80){

  if(conf >= 1 | conf <= 0){
    stop("conf must be between 0 and 1")
  }

  chisq <- qchisq(p = conf, df = 2*r+2, lower.tail = TRUE)
  duration <- mtbf*chisq/2
  return( data.frame(Failures = r, Duration = duration) )
}