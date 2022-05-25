#' CI for Operational Availability as given by Keesee
#'
#' \code{ao_keesee} gives a 2-sided CI for Operational Availability.
#'   The method was documented by W.R. Keesee in "A Method of Determining
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
#'   \code{\link{ao_keesee_test_duration}}
#'
#' @examples
#' # What is the 90% 2-sided Ao CI given 14 failures, 20 repairs,
#'   # 200 hours of Up Time and 100 hours of Down Time?
#' ao_keesee(n = 14, m = 20, upt = 200, dnt = 100, alpha = .2)
#'
#' @references
#' Keesee, W.R. A Method of Determining a Confidence Interval for Availability. 1965. url{https://apps.dtic.mil/sti/citations/AD0617716}
#'
#' @export
ao_keesee <- function(n, m, upt, dnt, alpha = .4){

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