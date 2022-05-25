#' Solve for one unknown in the Operational Availability formula
#'
#' \code{ao_unknown} solves for the unknown in the Operational Availability
#'   equation given the other inputs. One and only one of the parameters
#'   should be passed as \code{NA}.
#'
#' @param ao Operational Availability.
#' @param upt Up Time.
#' @param cmt Corrective Maintenance Time. 
#' @param aldt Administrative and Logistics Down Time.
#'
#' @return The output will be a numeric value for the unput passed as NA.
#'
#' @seealso \code{\link{ai_from_ao}}, \code{\link{ao_keesee}},
#'   \code{\link{ao_keesee_test_duration}}
#'
#' @examples
#' # What is the required MTBF to have a 90% prob of
#'   # completing a 24 hour mission duration?
#' ao_unknown(ao = 0.83, upt = 250, cmt = NA, aldt = 40.4)
#'
#' @export
ao_unknown <- function(ao = NA, upt = NA, cmt = NA, aldt = NA){
  if( sum(is.na(c(ao, upt, cmt, aldt))) != 1 ){
    stop("stop exactly one of ao, upt, cmt, aldt should = NA")
  }

  if(is.na(ao)){
    return( upt/(upt+cmt+aldt) )
  } else
  if(is.na(upt)){
    return( (ao*(cmt+aldt))/(1-ao) )
  } else
  if(is.na(cmt)){
    return( (upt/ao)-(upt+aldt) )
  } else
  if(is.na(aldt)){
    return( (upt/ao)-(upt+cmt) )
  }
  
}