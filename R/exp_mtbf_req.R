#' Calculates the Mean Time Between Failure (MTBF) Requirement
#'
#' \code{mtbf_req} calculates the Mean Time Between Failure
#'   (MTBF) requirement based on a defined Reliability requirement,
#'   and mission duration. (Instead of Time, Miles, Rounds, etc. may be used.)
#'   This assumes the times (miles, rounds, etc.) between failure
#'   are exponentially distributed.
#'
#' @param R_m A numeric mission Reliability requirement (0.00:1.00).
#' @param md A numeric mission duration. This could be time, miles, rounds, etc. 
#'
#' @return The output will be a numeric vector with units the same
#'   as the mission duration.
#'
#' @seealso \code{\link{exp_reliability_req}}, \code{\link{exp_test_duration}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{exp_test_demo}}
#'
#' @examples
#' # What is the required MTBF to have a 90% prob of
#'   # completing a 24 hour mission duration?
#' exp_mtbf_req(R_m = .9, md = 24)
#'
#' @export
exp_mtbf_req <- function(R_m, md){
  if(R_m >=1 | R_m <= 0){
    stop("R_m must be a probability between 0 and 1")
  }

  -md/log(R_m)
}