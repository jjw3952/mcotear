#' Maximum Likelihood Estimates for a Power Law Process
#'
#' \code{power_law_process} calculates the Maximum Likelihood Estimates
#'   for a Nonhomogeneous Poisson Process (NHPP) with Power Law Intensity
#'   Function (Crow-AMSAA model).
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param T A list of Total Time on Test (TTT) (i.e. test duration) vectors.
#'   The vectors in the list should be of length 1, and each vector should
#'   indicate a different system, i.e. if you have multiple systems each
#'   systems' TTT should be in it's own vector.
#' @param alpha 1-confidence, supplied for parameter confidence intervals.
#' @param fail.trunc Logical indicating if the test was failure terminated.
#' @param iter The number of iterations for parameter calculations when
#'   fail.truc is TRUE.
#'
#' @return The output will be a list consisting of parameter estimates,
#'   confidence interals (exact confidence intervals for beta), and iterative beta
#'   and lambda estimates to verify convergence when fail.truc is TRUE.
#'
#' @seealso \code{\link{power_law_mcf}}, \code{\link{mcf}},
#'   \code{\link{trend_test}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems all time truncated at 200 hours
#' power_law_process(
#'  t = split(amsaa$Time, amsaa$System),
#'  T = list(200,200,200),
#'  alpha = 0.05,
#'  fail.trunc = FALSE,
#'  iter = 10
#' )
#'
#' # Three systems all failure truncated
#' power_law_process(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(197.2,190.8,195.8),
#'   alpha = 0.19,
#'   fail.trunc = TRUE,
#'   iter = 10
#' )
#'
#' # One system time truncated at 200 hours
#' power_law_process(
#'   t = list(subset(amsaa, System == "S1")$Time),
#'   T = list(200),
#'   alpha = 0.05,
#'   fail.trunc = FALSE,
#'   iter = 10
#' )
#'
#' @export
power_law_process <- function(t, T, alpha = 0.05, fail.trunc = FALSE, iter = 10){
  # t = Time of Failure (not time between)
  # T = Total Time on Test
  ## MLEs, beta is shape, lambda is scale

  # Total number of failures
  #(n <- length(unlist(t)))
  (n <- sum(!is.na(unlist(t))))

  # Maximum likelihood estimate for beta
  #(beta.hat <- n / sum(
  #  rep(unlist( lapply(T, log)) , times = unlist( lapply(t, length) )) - 
  #  unlist( lapply(t, log) )
  #))
  (beta.hat <- n / sum(
    rep(
      unlist( lapply(T[!is.na(t)], log)),
      times = unlist( lapply(t[!is.na(t)], length) )) - 
    unlist( lapply(t[!is.na(t)], log) )
  ))

  # This works but I expect it to be slower ----------
  #  d <- NULL
  #  for( i in seq_along(t)){
  #    d[i] <- sum( log( T[[i]]/t[[i]] ) )
  #  }
  #  (beta.hat <- n/sum(d))
  #---------------------------------------------------

  if(fail.trunc == TRUE | (sum(is.na(unlist(t))) > 0) ){
    lambda.hat <- NULL

    # Iteratively solving for lambda.hat & beta.hat for failure truncated tests
    for(i in 1:iter){
      lambda.hat[i] <- n / sum( unlist(T)^beta.hat[length(beta.hat)] )

      beta.hat[i] <- n / (lambda.hat[length(lambda.hat)] *
        sum( unlist(T)^beta.hat[length(beta.hat)] * log(unlist(T)) ) - 
        sum(log(unlist(t[!is.na(t)]))))
    }

  } else
  if(fail.trunc == FALSE){
    # lambda.hat for time truncated tests
    lambda.hat <- n / sum( unlist(T)^beta.hat )
  }

  # Implementing the transformation the JMP & Minitab make
  (lambda.hat2 <- 1/exp(log(lambda.hat[length(lambda.hat)])/
    beta.hat[length(beta.hat)]))

  # CIs for beta from Statistical Methods for the Reliability of Repairable Systems
  # pgs 138 (time truncated), and 120 (failure truncated)
  if(fail.trunc == TRUE){
    (beta.lb <- (qchisq(1-alpha/2, 2*(n-1), lower.tail = FALSE)*beta.hat[length(beta.hat)]) / (2*n))
    (beta.ub <- (qchisq(1-alpha/2, 2*(n-1), lower.tail = TRUE)*beta.hat[length(beta.hat)]) / (2*n))
  } else {
    (beta.lb <- (qchisq(1-alpha/2, 2*n, lower.tail = FALSE)*beta.hat[length(beta.hat)]) / (2*n))
    (beta.ub <- (qchisq(1-alpha/2, 2*n, lower.tail = TRUE)*beta.hat[length(beta.hat)]) / (2*n))
  }
 

  # Simultaneous CIs on lambda.hat and beta.hat --------------------------------
  # This is based on pages 21-23 of AMSAA Technical Report 138
  # These are correct for beta.lb, beta.ub, lambda.lb, and lambd.ub,
  # but translating from lambda.lb and lambd.ub, to
  # lambda.lb2 and lambd.ub2 may not be correct
#  alpha.adj <- 1 - sqrt(1-alpha)

#  beta.lb2 <- beta.hat[length(beta.hat)] *
#    qchisq(alpha.adj/2, 2*n, lower.tail = TRUE) / (2*n)

#  beta.ub2 <- beta.hat[length(beta.hat)] *
#    qchisq(1-alpha.adj/2, 2*n, lower.tail = TRUE) / (2*n)

#  if(fail.trunc == FALSE){
#    lambda.lb <- qchisq(alpha.adj/2, 2*n, lower.tail = TRUE) /
#      (2*sum(unlist(T)^beta.ub2) )
#    lambda.ub <- qchisq(1-alpha.adj/2, 2*n+2, lower.tail = TRUE) /
#      (2*sum(unlist(T)^beta.lb2) )
#  } else
#  if(fail.trunc == TRUE){
#    lambda.lb <- qchisq(alpha.adj/2, 2*n) / (2* sum( unlist(T)^beta.ub2 ))
#    lambda.ub <- qchisq(1-alpha.adj/2, 2*n) / (2* sum( unlist(T)^beta.lb2 ))
#  }

  # Note sure if these are not correct
#  (lambda.lb2 <- 1/exp(log(lambda.lb)/beta.ub2))
#  (lambda.ub2 <- 1/exp(log(lambda.ub)/beta.lb2))

  #-----------------------------------------------------------------------------

  # The value of the shape (beta) depends on whether your
  #system is improving, deteriorating, or remaining stable.
    # If 0 < beta < 1, the failure/repair rate is decreasing.
    #Thus, your system is improving over time. 

    # If beta = 1, the failure/repair rate is constant.
    #Thus, your system is remaining stable over time. 

    # If beta > 1, the failure/repair rate is increasing.
    #Thus, your system is deteriorating over time. 

  # Putting the parameter CIs into a data.frame
  ci.df <- data.frame(
        lcb = c(beta.lb), #, lambda.lb2),
        ucb = c(beta.ub) #, lambda.ub2)
      )
  #row.names(ci.df)[1:2] <- c("beta","lambda")
  row.names(ci.df)[1] <- c("beta")

  # unbiased beta
  unbiased.beta <- ((n-1)*beta.hat[length(beta.hat)])/n

  return(
    list(
      "estimates" = c(
        "lambda" = lambda.hat2,
        "beta" = beta.hat[length(beta.hat)]
      ),
      "unbiased.beta" = unbiased.beta,
      "CIs" = ci.df,
      "beta.convergence" = beta.hat,
      "lambda.convergence" = lambda.hat
    )
  ) #End of Return
}