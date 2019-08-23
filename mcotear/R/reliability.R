#' Calculates the Mean Time Between Failure (MTBF) Requirement
#'
#' \code{mtbf_req} calculates the Mean Time Between Failure
#'   (MTBF) requirement based on a defined Reliability requirement,
#'   and mission duration. (Instead of Time, Miles, Rounds, etc. may be used.)
#'   This assumes the times (miles, rounds, etc.) between failure
#'   are exponentially distributed.
#'
#' @param r A numeric Reliability requirement (0.00:1.00).
#' @param md A numeric mission duration. This could be time, miles, rounds, etc. 
#'
#' @return The output will be a numeric vector with units the same
#'   as the mission duration.
#'
#' @seealso \code{\link{reliability_req}}, \code{\link{test_duration}},
#'   \code{\link{exp_mean_lcb}}
#'
#' @examples
#' # What is the required MTBF to have a 90% prob of
#'   # completing a 24 hour mission duration?
#' mtbf_req(r = .9, md = 24)
#'
#' @export
mtbf_req <- function(r, md){
  -md/log(r)
}




#' Calculates the Reliability Requirement
#'
#' \code{reliability_req} calculates the Reliability requirement based on a defined
#'   Mean Time Between Failure (MTBF) (really we use Mean Time Between
#'   Operational Mission Failure (MTBOMF)) requirement, and mission duration.
#'   (Instead of Time, Miles, Rounds, etc. may be used.)
#'   This assumes the times (miles, rounds, etc.) between failure
#'   are exponentially distributed.
#'
#' @param mtbf A numeric MTBF requirement. This could be time, miles, rounds,
#'   etc. Units should match the \code{md} parameter.
#' @param md A numeric mission duration. This could be time, miles, rounds,
#'   etc. Units should match the \code{mtbf} parameter.
#'
#' @return The output will be a numeric vector defining the Reliability
#'   requirement, (the probability that no failure is observed during
#'   the given mission duration).
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{test_duration}},
#'   \code{\link{exp_mean_lcb}}
#'
#' @examples
#' # What is the required Reliability if the MTBF is 228 hours and
#'   # a mission duration of 24 hours?
#' reliability_req(mtbf = 228, md = 24)
#'
#' @export
reliability_req <- function(mtbf, md){
  exp(-md/mtbf)
}






#' Calculates the required test length to meet a MTBF (Reliability) requirement
#'
#' \code{test_duration} calculates the required test duration (hours, miles,
#'   rounds, etc.) needed to meet a Mean Time Between Failure (Reliability)
#'   requirement given an allowable number of failures, a given level of
#'   confidence, and under the assumptions of exponentially distributed times
#'   between failures, and a time terminated test.
#'
#' @param n A numeric vector indicating the allowable number of failures.
#' @param mtbf A numeric Mean Time Between Failure requirement.
#'   This could be time, miles, rounds, etc.
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a two column \code{data.frame}
#'   with one column as the number of failures, 
#'   and the second column defining the required test
#'   duration (in the same units as the \code{mtbf} parameter), needed
#'   to satisfy the MTBF requirement with the given level of confidence,
#'   and the allowable number of failures.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{exp_mean_lcb}}
#'
#' @examples
#' # What is the required test duration to demonstrate the MTBF is at least
#'   # 228 hours, given 0:5 failures, and 80% confidence?
#' test_duration(n = 0:5, mtbf = 228, conf = 0.80)
#'
#' @export
test_duration <- function(n, mtbf, conf = 0.80){
  chisq <- qchisq(p = conf, df = 2*n+2, lower.tail = TRUE)
  duration <- mtbf*chisq/2
  return( data.frame(Failures = n, Duration = duration) )
}





#' Calculates the lower confidence bound for an exponential mean
#'
#' \code{exp_mean_lcb} calculates the lower confidence bound (LCB)
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), confidence level, and assuming the test is
#'   time terminated.
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
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}
#'
#' @examples
#' # What is the 80% LCB for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 367 hours, and 0 failures.
#' exp_mean_lcb(n = 0, duration = 367, conf = 0.80)
#'
#' @export
exp_mean_lcb <- function(n, duration, conf = 0.80){
  chisq <- qchisq(p = conf, df = 2*n+2, lower.tail = TRUE)
  2*duration/chisq
}