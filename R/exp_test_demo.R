#' Reliability Test Length from Null and Alternative Hypotheses, alpha, and beta
#'
#' \code{exp_test_demo} calculates the required test length, and allowable number
#'   of failures for a reliability demonstration test, given null and alternative
#'   hypotheses for MTTF/MTBF, alpha and beta error rates, and assuming the
#'   times are exponentially distributed. Hypothesis should be of the form,
#'   h0: MTBF <= X; h1: MTBF > X+Y, where X and Y are both positive values.
#'   This function comes from the AFIT COEF Reliability Test Planning for Mean
#'   Time Between Failures V2 document. Visit \url{https://www.afit.edu/stat/}
#'
#' @param mtbf0 A numeric value indicating the null hypothesis for MTBF/MTTR.
#' @param mtbf1 A numeric value indicating the alternative hypothesis for MTBF/MTTR.
#'   (hours, miles, rounds, etc.).
#' @param alpha The desired level of significance (type 1 error rate) (values
#'   within 0.0:1.00, default is set to 0.20).
#' @param beta The maximum allowable type II error rate (values
#'   within 0.0:1.00, default is set to 0.20).
#'
#' @return The output is a list supplying required test length (T),
#'   allowable number of failures (Accept), the number failures at which you
#'   would fail to reject the null hypothesis (i.e. you would reject the system) (Reject),
#'   and true type II error rate (Beta) (type II error rate = probability
#'   the null hypothesis is not rejected when it is false).
#'
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{exp_rel_power}}, \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @references
#' FIT COEF Reliability Test Planning for Mean Time Between Failures V2 \url{https://www.afit.edu/stat/}
#'
#' @examples
#' # What is the required test length and allowable number of failures,  
#'   # to demonstrate MTBF > 180 with confidence and ensuring there
#'   # is 80% power we accept a system with MTBF > 300
#'   # (assuming the times between failure are exponentially distributed)?
#' exp_test_demo(mtbf0 = 180, mtbf1 = 300, alpha = 0.2, beta = 0.2)
#'
#' @keywords internal
#' @export
exp_test_demo <- function(mtbf0, mtbf1, alpha = 0.2, beta = 0.2){

  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0){
    stop("alpha and beta must both be between 0 and 1")
  }

  r <- 0
  while(
    qchisq(beta, 2*(r+1))/qchisq(alpha, 2*(r+1), lower.tail = FALSE) < (mtbf0/mtbf1)
  ) { r <- r + 1 }
 
  T <- mtbf0 * .5 * qchisq(alpha, 2*(r+1), lower.tail = FALSE)
  Beta <- pchisq(
    mtbf0/mtbf1 * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1))
  list(T=T,Accept=r,Reject=r+1,Beta=Beta)
}