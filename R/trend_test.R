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
#' # One system, time truncated
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
  # The following would also give the same result
  # ad <- NULL
  # for(i in seq_along(T)){
  #  ad <- c(ad, log(T[[i]]/t[[i]]))
  #  }
  #  ad.test(ad, "pexp")

  if(fail.trunc == FALSE){
    scaled_ttt <- c(scaled_ttt, 1)
  }
  N <- length(scaled_ttt)

  sumAD <- NULL
  for( i in 1:(N-1)){
    sumAD[i] <- (2*i-1)*( log(scaled_ttt[i]) + log(1 - scaled_ttt[N-i]) );
  }
  (AD <- -(N-1) -(1/(N-1))*sum(sumAD))
  ad.p.value <- goftest::pAD(AD, n = N, lower.tail = FALSE)
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