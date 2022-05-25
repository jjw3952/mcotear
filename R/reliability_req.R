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
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}
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