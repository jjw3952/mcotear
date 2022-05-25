#' Calcualtes the confidence that we've met the reliability requirement
#'
#' \code{rel_conf} calculates the confidence that we've met the reliability
#'   requirement (the true MTBF exceeds the MTBF requirement), assuming that
#'   the times between falilure are exponentially distributed.
#'
#' @param mtbf_req A numeric vector indicating the MTBF requirement.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param r A numeric vector indicating the observed number of failures.
#'
#' @return The output will be a numeric vector, indicating the confidence
#'   that the MTBF requirement was met. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_mean_ci}},
#'   \code{\link{exp_mean_lcb}}
#'
#' @examples
#' # How much confidence do we have that the MTBF exceeds 250 hours, given
#'   # that we tested for 500 hours and observed 0 failures
#' rel_conf(250, 500, 0)
#'
#' @export
rel_conf <- function(mtbf_req, duration, r){
  pchisq( (2*duration)/mtbf_req, 2*(r+1), lower.tail = TRUE)
}