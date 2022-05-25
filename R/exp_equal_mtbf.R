#' Compare MTBFs for Exponential Distributions
#'
#' \code{exp_equal_mtbf} compares MTBF parameters from exponential distributions
#'   and returns the p-value.
#'
#' @param T A vector with test times, with the same length as r.
#' @param r A vector of failures, with the same length at T.
#'
#' @return The output is a list with the p-value and interpretation help. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{test_demo}}
#'
#' @examples
#'   # from pg 189-192 of Statistical Methods for the Reliability of
#'    # Repairable Systems; Rigon and Basu
#'   exp_equal_mtbf(T = c(152, 122), r = c(13, 7))
#'   exp_equal_mtbf(T = c(152, 122), r = c(13))
#'   exp_equal_mtbf(T = c(493, 687, 456, 722), r = c(6, 6, 6, 6))
#'
#' @references
#' Rigdon, Steven E., and Basu, Asit P. Escobar. Statistical Methods for the Reliability of Repairable Systems. Wiley-Interscience, 2000. (pgs 215-218)
#'
#' @export
exp_equal_mtbf <- function(T, r){

  if(length(T) != length(r)) {
    return("Error: T and r must be of the same length")
    break
  }
  
  mtbf <- T/r

  if(length(T) == 2){
    pvalue <- 2*pf(mtbf[1]/mtbf[2], 2*r[1], 2*r[2])
  }

  if(length(T) > 2){
    # -2 loglikelihood ratio
    (n2llr <- (2 * sum(r)* log( sum(T)/sum(r) )) - (2 * sum( r * log(mtbf) )))
    (pvalue <- pchisq(n2llr, (length(mtbf)-1), lower.tail = FALSE))
  }

  return(
    list(
     "pvalue" = pvalue,
     "interpretation" = "large p-values reject null hypothesis of equal MTBFs"
    )
  )
}