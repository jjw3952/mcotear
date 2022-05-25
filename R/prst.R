#' Probability Ratio Sequential Test (PRST)
#'
#' \code{prst} returns parameters defining a PRST as developed by Abraham Wald.
#'
#' @param MTBF0 The minimum acceptable MTBF.
#' @param MTBFA A value of MTBF greater than MTBF0 at which we want to
#'   ensure we don't reject the system.
#' @param alpha The producer's risk of rejecting equipment with MTBF > MTBFA
#'   (the probility of rejecting good equipment). Default = 0.10.
#' @param beta The consumer's risk of accepting equipment with MTBF < MTBF0.
#'   (the probility of accepting bad equipment). Default = 0.10.
#'
#' @return The output will return \code{r} = the number of failures at which
#'   the test terminated, \code{T} = the time at which the test is terminated,
#'   \code{a} = y-intercept for the accept time, \code{b} = slope of the accept
#'   and reject lines, \code{c} = y-intercept of the reject line, \code{df} =
#'   a \code{data.frame} giving the number of failures and the associated time
#'   times at which the test is terminated and the equipment accepted or rejected,
#'   as well as some text which outputs the form of the accept/reject lines, and
#'   the accept/reject times.
#'
#' @seealso \code{\link{reliability_req}}, \code{\link{test_duration}},
#'   \code{\link{test_demo}}, \code{\link{prst_plot}}
#'
#' @references
#' Mil-Hdbk-781A
#' 
#' Wald, Abraham. Sequential Analysis. John Wiley & Sons, 1947.
#' 
#' Brazovsky, Igor. Reliability Theory and Practice. Prentice Hall, 1961.
#'
#' @examples
#' prst(MTBF0=100, MTBFA=200, alpha=.1, beta=.1)
#' prst(MTBF0=100, MTBFA=200, alpha=.2, beta=.1)
#'
#' @export
prst <- function(MTBF0, MTBFA, alpha=.1, beta=.1){

  if(MTBF0 >= MTBFA){
    stop("MTBFA must be > MTBF0")
  }
  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0){
    stop("alpha and beta must both be between 0 and 1")
  }

  d <- MTBFA/MTBF0
  (A <- ((d+1)*(1-beta))/(2*alpha*d))
  (B <- beta/(1-alpha))

  (a <- log(B)/log(d))
  (b <- (1/MTBF0 - 1/MTBFA) / log(d))
  (c <- log(A) / log(d))

  r <- 1
  while(
    qchisq(alpha, 2*r, lower.tail = FALSE)/qchisq(beta, 2*r) > d
  ) { r <- r + 1 }
  # r = Truncation Failures

  # Truncation Time
  (T <- ceiling(MTBFA * qchisq(alpha, 2*r, lower.tail = TRUE) / 2))
  
  # The accept and reject times for r failures
  r_seq <- 0:r
  accept_time <- (r_seq-a)/b
  reject_time <- (r_seq-c)/b
  reject_time[reject_time < 0] <- NA
  accept_time[accept_time > T] <- NA
  df <- data.frame(r = r_seq, accept_le_r = accept_time, reject_ge_r = reject_time)

  #test_duration(r_seq, MTBF0, 1-alpha)

  return(
    list(
      r = r, T = T, a = a, b = b, c = c, df = df,
      reject_line = "r=c+b*t", accept_line = "r=a+b*t",
      reject_time = "t=(r-c)/b", accept_time = "t=(r-a)/b"
    )
  )
}