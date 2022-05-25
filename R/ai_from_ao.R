#' Solve for Inherent Availability Given Operational Availability, CMT and ALDT
#'
#' \code{ai_from_ao} solves for Inherent Availability
#'   given the other inputs.
#'
#' @param ao Operational Availability.
#' @param cmt Corrective Maintenance Time. 
#' @param aldt Administrative and Logistics Down Time.
#'
#' @return The output will be a numeric value for Inherent Availability.
#'
#' @seealso \code{\link{ao_unknown}}, \code{\link{ao_keesee}},
#'   \code{\link{ao_keesee_test_duration}}
#'
#' @examples
#' # What would inherent availability be if operational availability was 0.83,
#'  # CMT was 10.8 hours, and ALDT was 40.4 hours?
#' ai_from_ao(ao = 0.83, cmt = 10.8, aldt = 40.4)
#'
#' @export
ai_from_ao <- function(ao, cmt, aldt){
  ai <- (ao*cmt + ao*aldt)/(cmt + ao*aldt)
  return(ai)
}
