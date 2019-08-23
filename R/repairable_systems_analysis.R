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
#'   confidence interals (yet to be implemented), and iterative beta
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
  n <- length(unlist(t))

  # Maximum likelihood estimate for beta
  (beta.hat <- n / sum(
    rep(unlist( lapply(T, log)) , times = unlist( lapply(t, length) )) - 
    unlist( lapply(t, log) )
  ))

  # This works but I expect it to be slower ----------
  #  d <- NULL
  #  for( i in seq_along(t)){
  #    d[i] <- sum( log( T[[i]]/t[[i]] ) )
  #  }
  #  (beta.hat <- n/sum(d))
  #---------------------------------------------------

  if(fail.trunc == TRUE){
    lambda.hat <- NULL

    # Iteratively solving for lambda.hat & beta.hat for failure truncated tests
    for(i in 1:iter){
      lambda.hat[i] <- n / sum( unlist(T)^beta.hat[length(beta.hat)] )

      beta.hat[i] <- n / (lambda.hat[length(lambda.hat)] *
        sum( unlist(T)^beta.hat[length(beta.hat)] * log(unlist(T)) ) - 
        sum(log(unlist(t))))
    }

  } else
  if(fail.trunc == FALSE){
    # lambda.hat for time truncated tests
    lambda.hat <- n / sum( unlist(T)^beta.hat )
  }

  # Implementing the transformation the JMP & Minitab make
  (lambda.hat2 <- 1/exp(log(lambda.hat[length(lambda.hat)])/
    beta.hat[length(beta.hat)]))

  # Simultaneous CIs on lambda.hat and beta.hat --------------------------------
  # This is based on pages 21-23 of AMSAA Technical Report 138
  # These are correct for beta.lb, beta.ub, lambda.lb, and lambd.ub,
  # but translating from lambda.lb and lambd.ub, to
  # lambda.lb2 and lambd.ub2 is not correct
  alpha.adj <- 1 - sqrt(1-alpha)

  beta.lb <- beta.hat[length(beta.hat)] *
    qchisq(alpha.adj/2, 2*n, lower.tail = TRUE) / (2*n)

  beta.ub <- beta.hat[length(beta.hat)] *
    qchisq(1-alpha.adj/2, 2*n, lower.tail = TRUE) / (2*n)

  if(fail.trunc == FALSE){
    lambda.lb <- qchisq(alpha.adj/2, 2*n, lower.tail = TRUE) /
      (2*sum(unlist(T)^beta.ub) )
    lambda.ub <- qchisq(1-alpha.adj/2, 2*n+2, lower.tail = TRUE) /
      (2*sum(unlist(T)^beta.lb) )
  } else
  if(fail.trunc == TRUE){
    lambda.lb <- qchisq(alpha.adj/2, 2*n) / (2* sum( unlist(T)^beta.ub ))
    lambda.ub <- qchisq(1-alpha.adj/2, 2*n) / (2* sum( unlist(T)^beta.lb ))
  }
  #-----------------------------------------------------------------------------

  # These are not correct
  (lambda.lb2 <- 1/exp(log(lambda.lb)/beta.ub))
  (lambda.ub2 <- 1/exp(log(lambda.ub)/beta.lb))

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
        lcb = c(beta.lb, lambda.lb2),
        ucb = c(beta.ub, lambda.ub2)
      )
  row.names(ci.df)[1:2] <- c("beta","lambda")

  return(
    list(
      "estimates" = c(
        "lambda" = lambda.hat2,
        "beta" = beta.hat[length(beta.hat)]
      ),
      #"CIs" = ci.df,
      "beta.convergence" = beta.hat,
      "lambda.convergence" = lambda.hat
    )
  ) #End of Return
}





#' Nonparametric Estimate for the Mean Cumulative Function
#'
#' \code{mcf} calculates the sample nonparametric estimate of the mean
#'   cumulative function (expected number of failures at time t).
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param by If providing a list of length > 1 this can be
#'   a vector that defines a name for each element of the list
#'   so as to return by system mcf estimates.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the ordered failure times ("t") and corresponding nonparametric
#'    mcf estimate ("mcf").
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{trend_test}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems failure times.
#' mcf(t = split(amsaa$Time, amsaa$System))
#'
#' @export
mcf <- function(t, by = NULL){

  if(is.null(by)){

  (k <- length(t))         # number of systems
  t <- unlist(t, use.names = F)
  (n <- length(t))         # number of failures

  # Find position of failure times that occur more than once
  dups <- rev(which(duplicated(t[order(t)]) == TRUE))
  mcf <- seq(n/k/n, n/k, length.out = n)
  
  for(i in seq_along(dups)){
    mcf[dups[i]-1] <- mcf[dups[i]]
  }
  df <- data.frame(t = sort(t), mcf)


  } else{
  
  (k <- length(t))           # number of systems
  out <- rep(list(NULL), k)

  for(i in seq_along(1:k)){
    ti <- t[[i]]
    (n <- length(ti))         # number of failures

    # Find position of failure times that occur more than once
    dups <- rev(which(duplicated(ti[order(ti)]) == TRUE))
    mcf <- 1:n
  
    for(j in seq_along(dups)){
      mcf[dups[j]-1] <- mcf[dups[j]]
    }

    out[[i]] <- mcf
  }

  df <- data.frame(by = rep(by, lapply(out, length)), t = unlist(t), mcf = unlist(out))

  }

  return( df )
}








#' The Mean Cumulative Function for a Power Law Process
#'
#' \code{power_law_mcf} implements the mean cumulative function
#'   (expected number of failures at time t) for a Nonhomogeneous
#'   Poisson Process (NHPP) with Power Law Intensity Function
#'   (Crow-AMSAA model) given lambda (scale) and beta (shape)
#'   parameters/parameter estimates.
#'
#' @param t A vector or list of failure times. As with \code{t} in
#'   \code{\link{power_law_process}}, \code{t} could be a list of vectors
#'   such that each vector indicates a different system, i.e. if you have
#'   multiple systems each systems' failure times could be in it's own vector.
#' @param lambda the scale parameter or parameter estimate for a 
#'   Power Law NHPP. Can be calculated using \code{\link{power_law_process}}.
#' @param beta the shape parameter or parameter estimate for a 
#'   Power Law NHPP. Can be calculated using \code{\link{power_law_process}}.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the ordered failure times ("t") and corresponding Power Law
#'    mcf values ("power_mcf").
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{mcf}},
#'  \code{\link{trend_test}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#' data(cbPalette)
#'
#' # Three systems all time truncated at 200 hours
#'  # fit a NHPP Power Law (AMSAA-Crow) Model
#' (m <- power_law_process(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   alpha = 0.05,
#'   fail.trunc = FALSE,
#'   iter = 10))
#'
#' # Get the nonparametric mcf estimates
#'  # and change the name of "t" to "Time"
#'  # so it matches with the name in the 
#'  # amsaa data set
#' df_mcf <- mcf(t = split(amsaa$Time, amsaa$System))
#' names(df_mcf)[1] <- "Time"
#'
#' # Merge the nonparametric mcf estimates
#' # with amsaa into a new data.frame amsaa1
#' amsaa1 <- merge(amsaa, df_mcf, by = "Time")
#' head(amsaa1)
#'
#' # Either one of the following works
#' power_law_mcf(t = amsaa$Time, m$est[1], m$est[2])
#' power_law_mcf(t = split(amsaa$Time, amsaa$System), m$est[1], m$est[2])
#'
#' # Mean Cumulative Function Plot
#' theme_set(theme_bw())
#' ggplot(amsaa1, aes(x = Time, y = mcf)) +
#'   geom_point() +
#'   labs(
#'     x = "Operating Hours", y = "MCF",
#'     title = "Mean Cumulative Function") +
#'   stat_function(
#'     fun = function(x){
#'       power_law_mcf(t = x, lambda = m$e[[1]], beta = m$e[[2]])$power_mcf
#'     },
#'     mapping = aes(colour = "Power-Law NHPP")
#'   ) +
#'   scale_colour_manual("Functions:",
#'     breaks = c("Power-Law NHPP"),
#'     values = c("Power-Law NHPP" = cbPalette[2]),
#'     guide = guide_legend(
#'       override.aes = list(
#'         linetype = c("solid")
#'     ))
#'   ) +
#'   theme(legend.position = c(.175,.80),
#'     legend.background = element_rect(fill="grey95"),
#'     legend.key = element_rect(fill="grey95")
#'   )
#'
#' @export
power_law_mcf <- function(t, lambda, beta){
  # Mean Cumulative Function
  # Expected Number of Failures at Time t

  t <- sort(unlist(t, use.names = FALSE))
  mcf <- (t/lambda)^beta

  return( data.frame(t = t, power_mcf = mcf) )
}










#' Nonparametric Estimate for the Rate of Occurrence of Failures (ROCOF)
#'
#' \code{rocof} calculates the sample nonparametric estimate of the
#'   intensity function (i.e. rate of occurrence of failures (ROCOF), or failure rate)
#'   at time t. The inverse of this would be the sample nonparametric
#'   mean time between failure at time t.
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param by If providing a list of length > 1 this can be
#'   a vector that defines a name for each element of the list
#'   so as to return by system rocof and mtbf estimates.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the 'by' variable (which specifies the system or process the failuresm
#'   are attributable to, if supplied in the list name), the failure times ("t"),
#'   and the corresponding nonparametric estimates for the instantaneous
#'   rocof and mtbf.
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{trend_test}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems failure times.
#' rocof(t = split(amsaa$Time, amsaa$System))
#'
#' @export
rocof <- function(t, by = NULL){
  

  if(is.null(by)){
    tsort <- sort(unlist(t))
    mtbf <- tsort/(1:length(tsort))
    rocof <- mtbf^-1
    df <- data.frame(t = tsort, mtbf = mtbf, rocof = rocof)
  } else{
  
  if(length(by) != length(t)) stop("length(by) != length(t)")

  t <- lapply(t, sort)

  n <- lapply(t, length)
  mtbf <- lapply(t, function(x) x/(1:length(x)))
  rocof <- lapply(mtbf, `^`, -1)

  # If the name is NULL then make it the list indice number
  names <- lapply(seq_along(t), function(i) ifelse(is.null(names(t)[[i]]), i, names(t)[[i]]))
  names(t) <- unlist(names)

  by <- rep(names(t), n)
  t <- unsplit(t, f = by)
  mtbf <- unsplit(mtbf, f = by)
  rocof <- unsplit(rocof, f = by)
  df <- data.frame(by = by, t = t, mtbf = mtbf, rocof = rocof)
  }

  return(df)
}













#' The Intensity Function for a Power Law Process
#'
#' \code{power_law_intensity} implements the intensity function
#'   (i.e. failure rate, or rate of occurrence of failures (ROCOF))
#'   for a Nonhomogeneous Poisson Process (NHPP) with
#'   Power Law Intensity Function (Crow-AMSAA model) given lambda
#'   (scale) and beta (shape) parameters/parameter estimates.
#'
#' @param t A vector or list of failure times. As with \code{t} in
#'   \code{\link{power_law_process}}, \code{t} could be a list of vectors
#'   such that each vector indicates a different system, i.e. if you have
#'   multiple systems each systems' failure times could be in it's own vector.
#' @param lambda the scale parameter or parameter estimate for a 
#'   Power Law NHPP. Can be calculated using \code{\link{power_law_process}}.
#' @param beta the shape parameter or parameter estimate for a 
#'   Power Law NHPP. Can be calculated using \code{\link{power_law_process}}.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the ordered failure times ("t") and corresponding Power Law
#'    intensity values ("power_intensity").
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{mcf}},
#'  \code{\link{power_law_mcf}}, \code{\link{trend_test}},
#'  \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#' data(cbPalette)
#'
#' # Three systems all time truncated at 200 hours
#'  # fit a NHPP Power Law (AMSAA-Crow) Model
#' (m <- power_law_process(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   alpha = 0.05,
#'   fail.trunc = FALSE,
#'   iter = 10))
#'
#' # Get the nonparametric mcf estimates
#'  # and change the name of "t" to "Time"
#'  # so it matches with the name in the 
#'  # amsaa data set
#' df_mcf <- mcf(t = split(amsaa$Time, amsaa$System))
#' names(df_mcf)[1] <- "Time"
#'
#' # Merge the nonparametric mcf estimates
#' # with amsaa into a new data.frame amsaa1
#' amsaa1 <- merge(amsaa, df_mcf, by = "Time")
#' head(amsaa1)
#'
#' # Either one of the following works
#' power_law_intensity(t = amsaa$Time, m$est[1], m$est[2])
#' power_law_intensity(t = split(amsaa$Time, amsaa$System), m$est[1], m$est[2])
#'
#' @export
power_law_intensity <- function(t, lambda, beta){
  # Intensity Function, ROCOF
  # Expected Number of Failures at Time t

  t <- sort(unlist(t, use.names = FALSE))
  intensity <- (beta/lambda)*(t/lambda)^(beta-1)

  return( data.frame(t = t, power_intensity = intensity) )
}










#' Trend Tests for Repairable Systems Analysis
#'
#' \code{trend_test} tests to distinguish
#' between "no trend" and trends in Poisson Processes.
#' a trend following the Nonhomogeneous. \cr \cr
#' Laplace Centroid Test: Optimal for distinguishing between "no trend"
#' and a trend following the Nonhomogeneous Poisson Process (NHPP)
#' Exponential Law model. \cr \cr
#' Military Handbook Test: From Mil-HDBK-189, is optimal for
#' distinguishing between "no trend" and a trend following
#' the NHPP Power Law or Duane model. \cr \cr See:
#' \href{https://www.itl.nist.gov/div898/handbook/apr/section2/apr234.htm}{NIST Trend Tests} 
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
#' @return The output will be a list identifying the tests, test statistics,
#'   degrees of freedom (where applicable), and p-values.
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{mcf}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems all time truncated at 200 hours
#' trend_test(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   fail.trunc = FALSE)
#'
#' # Three systems all failure truncated
#' trend_test(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(197.2,190.8,195.8),
#'   fail.trunc = TRUE)
#'
#' #' # One system, time truncated
#' trend_test(
#'   t = list(subset(amsaa$Time, amsaa$System == "S1")),
#'   T = list(200),
#'   fail.trunc = FALSE)
#'
#' # One system, failure truncated
#' trend_test(
#'   t = list(subset(amsaa$Time, amsaa$System == "S1")),
#'   T = list(197.2),
#'   fail.trunc = TRUE)
#'
#' @export
trend_test <- function(t, T, fail.trunc = TRUE){
  # t = Time of Failure (not time between)
  # T = Total Time on Test
  # from "Tests for trend in more than one repairable system"
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.974&rep=rep1&type=pdf

  t <- lapply(t, sort)

  scaled_ttt <- ttt(t, T, fail.trunc)$scaled_ttt
  N <- length(scaled_ttt)

  #-------------------------------
  # Laplace TTT, only use when multiple systems
  (N <- ifelse(fail.trunc == TRUE, length(unlist(t)) - 1, length(unlist(t))))
  
   if(fail.trunc == TRUE){
    (Lt <- (sum(scaled_ttt[-length(scaled_ttt)]) - 0.5*N)/sqrt((1/12)*N))
   }
   if(fail.trunc == FALSE){
     (Lt <- (sum(scaled_ttt) - 0.5*N)/sqrt((1/12)*N))
   }


  (lt.p.value <- 2*pnorm(abs(Lt), lower.tail = F))
  #-------------------------------


  #-------------------------------
  # Laplace Combined

  (ni <- unlist( lapply(t, length) ))

  # If the test is failure truncated then need to
  # remove the last failures
  if(fail.trunc == TRUE){
    t_mod <- list() 
    for(i in seq_along(t)){
      t_mod[[i]] <- t[[i]][-ni[i]]
    }
    t <- t_mod
  }

  (ni <- unlist( lapply(t, length) ))
  (Ti <- unlist(T))

  (Lc <- (sum(unlist(t)) - sum(0.5 * ni * Ti)) /
    sqrt((1/12) * sum(ni*Ti^2)))

  (lc.p.value <- 2*pnorm(abs(Lc), lower.tail = F))
  #-------------------------------

  laplace.interp <- list(
    "Reject null hypothesis of constant failure rate if p.value is < alpha.",
    "If reject, indicates Loglinear NHPP.",
    "Negative score indicates decreasing failure rate.",
    "Positive score indiates increasing failure rate.")

  df.laplace <- data.frame(
    test = c("Laplace Combined", "Laplace TTT"),
    statistic = c(Lc, Lt),
    pval = c(lc.p.value, lt.p.value)
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  #===============================
  # Mil-HDBK-189 Test Combined

   Tvec <- unlist(T)
   mc <- NULL
   for(i in seq_along(unlist(T))){
     mc[i] <- sum(log( Tvec[i] / t[[i]] ))
   }

   (mc <- 2 * sum(mc))
   (dfc <- 2*length(unlist(t)))
   (mc.p.value <- 2*min(
    pchisq(mc, dfc, lower.tail = T),
    pchisq(mc, dfc, lower.tail = F)))
  #===============================

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Mil-HDBK-189 Test TTT

  (mt <- 2*sum(log(scaled_ttt^-1)))
  (dft <- 2*N)
   (mt.p.value <- 2*min(
    pchisq(mt, dft, lower.tail = T),
    pchisq(mt, dft, lower.tail = F)))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  mil.interp <- paste(
    "Reject null hypothesis of constant failure rate if p.value is < alpha.",
    "If reject, indicates Power NHPP.",
    "Large statistic indicates decreasing failure rate.", 
    "Small statistic indicates increasing failure rate.", sep="\n")

  df.mil <- data.frame(
    test = c("Mil-HDBK Combined", "Mil-HDBK TTT"),
    statistic = c(mc, mt),
    df = c(dfc, dft),
    pval = c(mc.p.value, mt.p.value)
  )


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Anderson Darling Test for Trend
  # https://uu.diva-portal.org/smash/get/diva2:468559/FULLTEXT01.pdf
  # Investigation of some tests for homogeneity of intensity
  # with applications to insurance data

  if(fail.trunc == FALSE){
    scaled_ttt <- c(scaled_ttt, 1)
  }
  N <- length(scaled_ttt)

  sumAD <- NULL
  for( i in 1:(N-1)){
    sumAD[i] <- (2*i-1)*( log(scaled_ttt[i]) + log(1 - scaled_ttt[N-i]) );
  }
  (AD <- -(N-1) -(1/(N-1))*sum(sumAD))
  ad.p.value <- goftest::pAD(AD, n = 500, lower.tail = FALSE)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  df.ad <- data.frame(
    test = "Anderson-Darling",
    statistic = AD,
    pval = ad.p.value
  )

  #return(list(statistic = statistic, p.value = p.value, interpretation = interp))
  return(
    list(
      "Mil-HDBK" = df.mil,#,
      #"Mil-HDBK Interpretation" = mil.interp
      "Laplace" = df.laplace,
      #"Laplace Interpretation" = laplace.interp,
      "Anderson-Darling" = df.ad
    )
  )
}






#' Total Time on Test for Repairable Systems
#'
#' \code{ttt} calculates the scaled total time on test (TTT) as described in
#' \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.974&rep=rep1&type=pdf}{Tests for trend in more than one repairable system}
#' and \href{www.dtic.mil/dtic/tr/fulltext/u2/a043573.pdf}{Analysis of Time Between Failures for Repairable Components} \cr \cr
#' As indicated in \href{www.dtic.mil/dtic/tr/fulltext/u2/a043573.pdf}{Analysis of Time Between Failures for Repairable Components}
#' this can be used to create a TTT plot. \cr \cr
#' A TTT plot that follows the line \eqn{y = x}
#' indicates a Homogeneous Poisson Process (HPP) (constant failure rate),
#' while a concave TTT plot indicates a Nonhomogeneous Poisson Process (NHPP).
#' Concave up indicates decreasing failure rate,
#' while concave down indicates increasing failure rate. See:
#' \href{http://www.pinzhi.org/Minitab/Reliability_and_Survival_Analysis/RS_Growth_Curves/Total_time_on_test_plot.htm}{Minitab TTT Plot}
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
#' @return The output will be a data.frame with the sorted supplied time
#'   values (\code{t}), the total time on test (\code{ttt}), and the
#'   scaled total time on test (\code{scaled_ttt}).
#'   A plot of \code{scaled_ttt} vs \code{ttt} would be a TTT plot.
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{mcf}}, \code{\link{trend_test}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems all time truncated at 200 hours
#' ttt_df <- ttt(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   fail.trunc = FALSE)
#'
#' theme_set(theme_bw())
#' ggplot(ttt_df, aes(x = ttt, y = scaled_ttt)) +
#'   geom_line(colour = "red") + geom_point() +
#'   geom_abline(intercept = 0, slope = 1) +
#'   labs(
#'     x = "Total Time on Test",
#'     y = "Scaled Total Time on Test")
#'
#' @export
ttt <- function(t, T, fail.trunc = FALSE){

  if(fail.trunc == TRUE){

    T <- unlist(T)
    tsort <- sort(unlist(t))
    test_time_accrued <- NULL
    N <- length(tsort)

    for(i in seq_along(tsort)){
      n <- length(which(T >= tsort[i]))
      test_time_accrued[i] <- tsort[i]*n
      if(i > 1){
        if(test_time_accrued[i] < test_time_accrued[i-1]){
          test_time_accrued[i] <- test_time_accrued[i] + sum(T[which(T < tsort[i])])
        }
      }
    }
    scaled_ttt <- test_time_accrued / max(test_time_accrued)

    df <- data.frame(t = tsort, ttt = (1:N)/N, scaled_ttt = scaled_ttt)
  }
  if(fail.trunc == FALSE){
  
    T <- unlist(T)
    tsort <- sort(unlist(t))
    test_time_accrued <- NULL
    N <- length(tsort)

    for(i in seq_along(tsort)){
      n <- length(which(T >= tsort[i]))
      test_time_accrued[i] <- tsort[i]*n
      if(i > 1){
        if(test_time_accrued[i] < test_time_accrued[i-1]){
          test_time_accrued[i] <- test_time_accrued[i] + sum(T[which(T < tsort[i])])
        }
      }
    }

    # Add the final times (when all the systems stopped testing)
    test_time_accrued <- c(test_time_accrued, sum(T))

    scaled_ttt <- test_time_accrued / max(test_time_accrued)

    df <- data.frame(t = tsort, ttt = (1:N)/N, scaled_ttt = scaled_ttt[-(N+1)])
  }

  return(df)
}






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