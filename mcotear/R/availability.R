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
#'\
#' @seealso \code{\link{ai_from_ao}}, \code{\link{keese_ao}},
#'   \code{\link{keese_test_duration}}
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
#' @seealso \code{\link{ao_unknown}}, \code{\link{keese_ao}},
#'   \code{\link{keese_test_duration}}
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





#' CI for Operational Availability as given by Keese
#'
#' \code{keese_ao} gives a 2-sided CI for Operational Availability.
#'   The method was documented by W.R. Keese in "A Method of Determining
#'   a Confidence Interval for Availability".
#'
#' @param n Number of failures.
#' @param m Number of repairs.
#' @param upt Up Time. 
#' @param dnt Down Time.
#' @param alpha Two-sided alpha value.
#'
#' @return The output will be a numeric vector giving the two-sided CI for
#'   Operational Availability, and point estimate.
#'
#' @seealso \code{\link{ao_unknown}}, \code{\link{ai_from_ao}},
#'   \code{\link{keese_test_duration}}
#'
#' @examples
#' # What is the 90% 2-sided Ao CI given 14 failures, 20 repairs,
#'   # 200 hours of Up Time and 100 hours of Down Time?
#' keese_ao(n = 14, m = 20, upt = 200, dnt = 100, alpha = .2)
#'
#' @export
keese_ao <- function(n, m, upt, dnt, alpha = .4){

  if(alpha >= 1 | alpha <= 0){
    stop("alpha must be between 0 and 1")
  }

  mtbf <- upt/n
  mttr <- dnt/m
  k <- (n+1)/n
  df1 <- 2*(n+1)
  df2 <- 2*m
  cv1 <- qf(alpha/2, df1, df2, lower.tail = FALSE)
  cv2 <- qf(alpha/2, df1, df2, lower.tail = TRUE)
  bu <- (mttr/mtbf)*k*cv2
  bl <- (mttr/mtbf)*k*cv1
  lwr <- 1/(1+bl)
  upr <- 1/(1+bu)
  ao <- upt/(upt+dnt)
  out <- c(lwr, ao, upr)
  lwr_p <- alpha/2
  names(out) <- c(lwr_p, "ao", 1-lwr_p)
  return(out)
}





#' Determines the test time required to demonstrate Availability was met with Confidence
#'
#' \code{keese_test_duration} determines the test time required to demonstrate
#'    Availability was met with confidence, assuming a nunmber of failures, repairs,
#'   and mean time to repair (MTTR).
#'
#' @param n Number of failures.
#' @param m Number of repairs.
#' @param mttr Assumed mean time to repair. 
#' @param ao Availability to be demonstrated.
#' @param alpha Two-sided alpha value.
#'
#' @return The output will be a numeric vector giving the required Up Time,
#'   assumed Down Time, and Total Time.
#'
#' @seealso \code{\link{ao_unknown}}, \code{\link{ai_from_ao}},
#'   \code{\link{keese_ao}}
#'
#' @examples
#' keese_test_duration(n=1, m=1, mttr=1, ao_lcb=.83, alpha = .4)
#' keese_test_duration(n=2, m=2, mttr=10.8+40.4, ao_lcb=.83, alpha = .4)
#' keese_test_duration(n=2, m=2, mttr=10.8, ao_lcb=0.9585852, alpha = .4)
#'
#' @export
keese_test_duration <- function(n, m, mttr, ao, alpha = .4){
  df1 <- 2*(n+1)
  df2 <- 2*m
  cv1 <- qf(alpha/2, df1, df2, lower.tail = FALSE)
  cv2 <- qf(alpha/2, df1, df2, lower.tail = TRUE)
  up <- (ao*cv1*(n+1)*mttr) / (1-ao_lcb)
  dn <- mttr*m
  total <- up+dn
  mtbf <- up/n
  out <- c(up, dn, total)
  names(out) <- c("upt", "dnt", "total")
  return(out)
}