#' Bartlett's Modified Likelihood Ratio Chi-Sq Test for Common Shape
#'
#' \code{common_beta} provides a test for a common shape parameter given multiple
#'   systems/processes assumed to come from a Nonhomogeneous Poisson Process (NHPP)
#'   following a Power Law Model (Crow-AMSAA model). Small p-values reject null
#'   hypothesis of common beta (shape paramter). See: 
#'   \href{https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/reliability/how-to/parametric-growth-curve/methods-and-formulas/tests-for-equal-shapes-or-scales/}{Minitab Methods and formulas for test for equal shapes or scales in Parametric Growth Curve}
#'   and \href{https://apps.dtic.mil/docs/citations/ADA020296}{AMSAA Technical Report No. 138 Reliability Analysis for Complex, Repairable Systems (L.H. Crow 1975)}.
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param T A list of Total Time on Test (TTT) (i.e. test duration) vectors.
#'   The vectors in the list should be of length 1, and each vector should
#'   indicate a different system, i.e. if you have multiple systems each
#'   systems' TTT should be in it's own vector.
#' @param fail.trunc Logical indicating if the test was failure terminated.
#'
#' @return The output will be a list with the test statistic, p-value, and
#'   degrees of freedom.
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{mcf}}, \code{\link{trend_test}}, \code{\link{ttt}}
#'
#' @examples
#' data(amsaa)
#'
#' common_beta(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200, 200, 200),
#'   fail.trunc = FALSE) 
#'
#' @export
common_beta <- function(t, T, fail.trunc = FALSE){
  k <- length(t)
  if(k < 1) stop("Less than two systems/process provided")

  if(fail.trunc == FALSE){
    (ni  <- lapply(t, length))
  }

  if(fail.trunc == TRUE){
    for(i in seq_along(t)){
      t[[i]] <- t[[i]][-length(t[[i]])]
    }
    (ni  <- lapply(t, length))
  } 

  # Get beta for each system/process by itself
  beta <- NULL
  for(i in seq_along(t)){
    beta[i] <- power_law_process(
      t = t[i],
      T = T[i],
      fail.trunc = FALSE)$est[[2]]
  }

#  if(k == 2){
#    test.stat <- beta[2]/beta[1]
#    p.value <- pf(test.stat, 2*ni[[1]], 2*ni[[2]], lower.tail = FALSE)
#  }
#  if(k > 2){
    M <- sum(unlist(ni))
    (beta.star <- power_law_process(
      t = t,
      T = T,
      fail.trunc = FALSE)$est[[2]])

    (L <- sum(unlist(ni) * log(beta)) - M * log(beta.star))
    a <- 1 + (1/(6*(k-1))) * (sum(unlist(ni)^-1) - M^-1)
    (D <- 2*L/a)

    p.value <- #min(
      pchisq(D, k-1, lower.tail = FALSE)#,
      #pchisq(D, k-1, lower.tail = TRUE))
#  }

  # The value given above is from Crow's AMSAA Technical Report 138 Dec 1975
  #   it is Bartlett's modified likelihood ratio chi-sq test for equal shape parameter

  # The following is the same thing, but a different method as
  # indicated on Minitab's website
  # https://support.minitab.com/en-us/minitab/18/help-and-how-to/
  # modeling-statistics/reliability/how-to/parametric-growth-curve/
  # methods-and-formulas/tests-for-equal-shapes-or-scales/

    num <- 1
    den <- 1
    # Calculate the numerator and denominator for the LR
    for(i in seq_along(beta)){
     #t[[i]] <- t[[i]][-length(t[[i]])]
     num <- num * prod(
              factorial(ni[[i]]),
              prod(beta[i] * (t[[i]]/T[[i]])^(beta[i] - 1))
            )
     den <-  den * prod(
              factorial(ni[[i]]),
              prod(beta.star * (t[[i]]/T[[i]])^(beta.star - 1))
            )
    }

    LR <- num/den

    (bart.test.stat <- 2*log(LR)/a)
    bart.p.value <- #min(
      pchisq(bart.test.stat, k-1, lower.tail = FALSE)#,
      #pchisq(bart.test.stat, k-1, lower.tail = TRUE))

  return(list(
#    beta = beta,
#    beta.star = beta.star,
    "Test Statistic" = D,
    "P-Value" = p.value,
    "df" = k-1)
  )

#  return(list(
#    beta = beta,
#    "Test Statistic" = bart.test.stat,
#    "P-Value" = bart.p.value)
#  )
}