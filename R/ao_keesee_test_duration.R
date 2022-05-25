#' Determines the test time required to demonstrate Availability was met with Confidence
#'
#' \code{ao_keesee_test_duration} determines the test time required to demonstrate
#'    Availability was met with confidence, assuming a nunmber of failures, repairs,
#'   and mean time to repair (MTTR) (an optionally Admin/Logistics Delay Time (ALDT),
#'   Mean ALDT (MLDT), and the number of observed ALDTs).
#'
#' @param n Number of failures.
#' @param m Number of repairs.
#' @param k Number of Admin/Logistics Delays (set to NA as default to exclude ALDT).
#' @param mttr Assumed mean time to repair. 
#' @param mldt Assumed mean logistics delay time (set to NA as default to exclude ALDT). 
#' @param ao Availability to be demonstrated.
#' @param alpha Two-sided alpha value.
#'
#' @return The output will be a numeric vector giving the required Up Time,
#'   assumed Down Time, and Total Time.
#'
#' @seealso \code{\link{ao_unknown}}, \code{\link{ai_from_ao}},
#'   \code{\link{ao_keesee}}
#'
#' @examples
#' ao_keesee_test_duration(n=1, m=1, mttr=1, ao_lcb=.83, alpha = .4)
#' ao_keesee_test_duration(n=2, m=2, mttr=10.8+40.4, ao_lcb=.83, alpha = .4)
#' ao_keesee_test_duration(n=2, m=2, mttr=10.8, ao_lcb=0.9585852, alpha = .4)
#' 
#' @references
#' Keesee, W.R. A Method of Determining a Confidence Interval for Availability. 1965. url{https://apps.dtic.mil/sti/citations/AD0617716}
#' 
#' @export
ao_keesee_test_duration <- function(n, m, k = NA, mttr, mldt = NA, ao, alpha = .4){

  if(alpha >= 1 | alpha <= 0){
    stop("alpha must be between 0 and 1")
  }

  if(is.na(k) | is.na(mldt)){
    df1 <- 2*(n+1)
    df2 <- 2*m
    cv1 <- qf(alpha/2, df1, df2, lower.tail = FALSE)
    cv2 <- qf(alpha/2, df1, df2, lower.tail = TRUE)
    up <- (ao*cv1*(n+1)*mttr) / (1-ao)
    dn <- mttr*m
    total <- up+dn
    out <- c(up, dn, total)
    names(out) <- c("upt", "dnt", "total")
    return(out)
  } else{
    df1 <- 2*(n+1)
    df2 <- 2*(m+k)
    cv1 <- qf(alpha/2, df1, df2, lower.tail = FALSE)
    cv2 <- qf(alpha/2, df1, df2, lower.tail = TRUE)
    up <- cv1*(mttr+mldt)*((ao*n*(n+1))/(n*(1-ao)))
    dn <- mttr*m+mldt*k
    total <- up+dn
    out <- c(up, dn, total)
    names(out) <- c("upt", "dnt", "total")
    return(out)
  }
}