#' Calculates the Mean Time Between Failure (MTBF) Requirement
#'
#' \code{mtbf_req} calculates the Mean Time Between Failure
#'   (MTBF) requirement based on a defined Reliability requirement,
#'   and mission duration. (Instead of Time, Miles, Rounds, etc. may be used.)
#'   This assumes the times (miles, rounds, etc.) between failure
#'   are exponentially distributed.
#'
#' @param r A numeric Reliability requirement (0.00:1.00).
#' @param md A numeric mission duration. This could be time, miles, rounds, etc. 
#'
#' @return The output will be a numeric vector with units the same
#'   as the mission duration.
#'
#' @seealso \code{\link{reliability_req}}, \code{\link{test_duration}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}}
#'
#' @examples
#' # What is the required MTBF to have a 90% prob of
#'   # completing a 24 hour mission duration?
#' mtbf_req(r = .9, md = 24)
#'
#' @export
mtbf_req <- function(r, md){
  -md/log(r)
}




#' Calculates the Reliability Requirement
#'
#' \code{reliability_req} calculates the Reliability requirement based on a defined
#'   Mean Time Between Failure (MTBF) (really we use Mean Time Between
#'   Operational Mission Failure (MTBOMF)) requirement, and mission duration.
#'   (Instead of Time, Miles, Rounds, etc. may be used.)
#'   This assumes the times (miles, rounds, etc.) between failure
#'   are exponentially distributed.
#'
#' @param mtbf A numeric MTBF requirement. This could be time, miles, rounds,
#'   etc. Units should match the \code{md} parameter.
#' @param md A numeric mission duration. This could be time, miles, rounds,
#'   etc. Units should match the \code{mtbf} parameter.
#'
#' @return The output will be a numeric vector defining the Reliability
#'   requirement, (the probability that no failure is observed during
#'   the given mission duration).
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{test_duration}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}
#'
#' @examples
#' # What is the required Reliability if the MTBF is 228 hours and
#'   # a mission duration of 24 hours?
#' reliability_req(mtbf = 228, md = 24)
#'
#' @export
reliability_req <- function(mtbf, md){
  exp(-md/mtbf)
}






#' Calculates the required test length to meet a MTBF (Reliability) requirement
#'
#' \code{test_duration} calculates the required test duration (hours, miles,
#'   rounds, etc.) needed to meet a Mean Time Between Failure (Reliability)
#'   requirement given an allowable number of failures, a given level of
#'   confidence, and under the assumptions of exponentially distributed times
#'   between failures, and a time terminated test.
#'
#' @param r A numeric vector indicating the allowable number of failures.
#' @param mtbf A numeric Mean Time Between Failure requirement.
#'   This could be time, miles, rounds, etc.
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a two column \code{data.frame}
#'   with one column as the allowable number of failures, 
#'   and the second column defining the required test
#'   duration (in the same units as the \code{mtbf} parameter), needed
#'   to satisfy the MTBF requirement with the given level of confidence,
#'   and the allowable number of failures.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @examples
#' # What is the required test duration to demonstrate the MTBF is at least
#'   # 228 hours, given 0:5 failures, and 80% confidence?
#' test_duration(r = 0:5, mtbf = 228, conf = 0.80)
#'
#' @export
test_duration <- function(r, mtbf, conf = 0.80){

  if(conf >= 1 | conf <= 0){
    stop("conf must be between 0 and 1")
  }

  chisq <- qchisq(p = conf, df = 2*r+2, lower.tail = TRUE)
  duration <- mtbf*chisq/2
  return( data.frame(Failures = r, Duration = duration) )
}





#' Calculates the lower confidence bound for an exponential mean
#'
#' \code{exp_mean_lcb} calculates the lower confidence bound (LCB)
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), number of failures, confidence level, and assuming
#'   the test is time terminated.
#'
#' @param n A numeric vector indicating the observed number of failures.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a numeric vector, indicating the LCB of
#'   the exponential mean with the given level of confidence. The units
#'   will be the same as the units of the supplied \code{duration}. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}
#'
#' @examples
#' # What is the 80% LCB for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 367 hours, and 0 failures.
#' exp_mean_lcb(n = 0, duration = 367, conf = 0.80)
#'
#' @export
exp_mean_lcb <- function(n, duration, conf = 0.80){

  if(conf >= 1 | conf <= 0){
    stop("conf must be between 0 and 1")
  }

  chisq <- qchisq(p = conf, df = 2*n+2, lower.tail = TRUE)
  2*duration/chisq
}





#' Reliability Test Length from Null and Alternative Hypotheses, alpha, and beta
#'
#' \code{test_demo} calculates the required test length, and allowable number
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
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{rel_power}}, \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @examples
#' # What is the required test length and allowable number of failures,  
#'   # to demonstrate MTBF > 180 with confidence and ensuring there
#'   # is 80% power we accept a system with MTBF > 300
#'   # (assuming the times between failure are exponentially distributed)?
#' test_demo(mtbf0 = 180, mtbf1 = 300, alpha = 0.2, beta = 0.2)
#'
#' @export
test_demo <- function(mtbf0, mtbf1, alpha = 0.2, beta = 0.2){

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





#' Power of a Reliability Test from Null and Alternative Hypotheses, alpha, and allowable failures
#'
#' \code{rel_power} calculates the power of a reliability test given null and alternative
#'   hypotheses for MTTF/MTBF, the allowable numbers, the alpha error rate, and assuming
#'   the times are exponentially distributed. Hypothesis should be of the form,
#'   h0: MTBF <= X; h1: MTBF > X+Y, where X and Y are both positive values.
#'
#' @param mtbf0 A numeric value indicating the null hypothesis for MTBF/MTTR.
#' @param mtbf1 A numeric value indicating the alternative hypothesis for MTBF/MTTR.
#'   (hours, miles, rounds, etc.).
#' @param r The allowable number of failures.
#' @param alpha The desired level of significance (type 1 error rate) (values
#'   within 0.0:1.00, default is set to 0.20) (producer's risk - the probability we reject
#'   a system with MTBF = mtbf1).
#'
#' @return The output is a numeric value representing the power of rejecting
#'   the null hypothesis. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{test_demo}}, \code{\link{exp_equal_mtbf}}, \code{\link{exp_oc}}
#'
#' @examples
#' # Assuming the true MTBF is 300, what is the power of test  
#'   # to demonstrate MTBF > 180, with 80% power and confidence, assuming 
#'   # the true MTBF is 300 (and assuming the times between failure are
#'   # exponentially distributed)
#' rel_power(mtbf0 = 180, mtbf1 = 300, r = 1, alpha = 0.2)
#'
#' mtbf1 <- seq(100, 600, 1)
#' df <- data.frame(
#'   r = rep(c(1:5, 11), each = length(mtbf1)),
#'   mtbf1 = mtbf1
#' )
#' df$power <- rel_power(mtbf0 = 180, mtbf1 = df$mtbf1, r = df$r, alpha = .2)
#' df$T <- test_duration(r = df$r, mtbf = 180, conf = .8)$Duration
#' df$label <- paste0("r = ", df$r, "; T = ", ceiling(df$T))
#' 
#' ggplot2::ggplot(df) + 
#'   ggplot2::geom_path(
#'     ggplot2::aes(x = mtbf1, y = power, colour = label)
#'   ) +
#'   ggplot2::scale_colour_manual("", values = cbbPalette) +
#'   ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   ggplot2::labs(
#'    y = "Probability of Passing Demonstration\n(Power)",
#'    x = "True MTBOMF",
#'    caption = "r = Allowable Failures\nT = Total Test Time Required (hours)\n\nConfidence set to 80%\nMTBF0 = 180 hours"
#'   ) +
#'   ggplot2::theme(plot.caption = element_text(hjust = 0))
#'
#' @export
rel_power <- function(mtbf0, mtbf1, r = 1, alpha = 0.20){
  pchisq(
    (mtbf0/mtbf1) * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1), lower.tail = FALSE
  )
}





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





#' Exponential Operating Characteristic (OC) Curves
#'
#' \code{exp_oc} computes OC curves for exponential reliability tests
#'   (probability of passing test as a function of true MTBF). See pg. 67
#'   for the OC formula in MIL-HDBK-781A.
#'
#'
#' @param accept A vector with the allowable number of failures.
#' @param duration A vector of test durations.
#' @param mtbf A vector of the true MTBFs.
#'
#' @return The probability of passing the test of a given duration with an
#'   allowable number of failures, given the true MTBF.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{test_demo}}
#'
#' @examples
#'   # Theta is MTBF, 0 is the number at which you accept - system passes test
#'   # 804.719 is the test duration
#'   theta <- seq(250, 5000, 50)
#'   prob_pass <- exp_oc(accept = 0, duration = 804.719, mtbf = theta)
#'   prob_pass[1:5]
#'
#'   ggplot2::ggplot(data.frame(prob_pass = prob_pass, theta = theta)) +
#'     ggplot2::geom_line(ggplot2::aes(x = theta, y = prob_pass)) +
#'     ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
#'
#' @export
exp_oc <- function(accept, duration, mtbf){
  prob_pass <- ppois(accept, duration/mtbf, lower.tail=TRUE)
  prob_pass
}





#' Exponential Fixed Duration Test Plans
#'
#' \code{fixed_duration_tests} See pg. 67 of MIL-HDBK-781A. Fixed duration
#'   tests for exponentially distributed MTBF with desired alpha and beta.
#'
#'
#' @param MTBF0 The MTBF below which to reject the system with probability alpha.
#' @param MTBFA The MTBF above which to accept the system with probability 1-beta.
#' @param alpha The type I error rate, producer's risk - the probability we reject
#'   a system with MTBF = MTBFA.
#' @param beta The type II error rate, consumer's risk - the probability we accept
#'   a system with MTBF = MTBF0.
#'
#' @return The output is a data.frame containing the allowable number of failures
#'   at which the system would pass the test (Accept), the number of failures at
#'   which the system would fail the test (reject), the test duration (Duration),
#'   and the actual beta value (the probability we accept a system with MTBF = MTBF0)
#'   given the proposed test.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{test_demo}}, \code{\link{exp_oc}}
#'
#' @examples
#'   (fd <- fixed_duration_tests(MTBF0 = 500, MTBFA = 1000, alpha = .2, beta = .2))
#'   theta <- seq(250, 2000, 50)
#'   prob_pass <- exp_oc(accept = fd$Accept[6], duration = fd$Duration[6], mtbf = theta)
#'
#'   df <- data.frame(x = c(500,1000), y = c(0.20, 1-fd$beta[6]), label = c("alpha", "1-beta"))
#'
#'   ggplot2::ggplot(data.frame(prob_pass = prob_pass, theta = theta)) +
#'     ggplot2::geom_point(ggplot2::aes(x = theta, y = prob_pass)) +
#'     ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'     ggplot2::geom_point(
#'       data = df,
#'       mapping = ggplot2::aes(x, y), colour = "red"
#'     ) +
#'     ggplot2::geom_text(
#'       data = df,
#'       mapping = ggplot2::aes(x, y, label = label), parse = TRUE, colour = "red", hjust = c(-1,1.2)
#'     )
#'
#' @export
fixed_duration_tests <- function(MTBF0 = 500, MTBFA = 1000, alpha = .2, beta = .2){

  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0){
    stop("alpha and beta must both be between 0 and 1")
  }

  a <- 0
  i <- 1
  T <- 0
  act_beta <- 1
  while(act_beta[i] > beta){
    #fn <- function(x) abs(beta -  ppois(a, lambda = x/MTBF0, lower.tail = TRUE))
    #(T <- optimize(fn, c(0,upr), tol = 1E-4)$min)

    #fn1 <- function(x) {alpha -  ppois(a[i], lambda = x/MTBF0, lower.tail = TRUE)}
    #(T[i+1] <- uniroot(fn1, c(0,upr), tol = 1E-4)$root)
    (T[i+1] <- test_duration(a[i], MTBF0, conf = 1-alpha)[[2]])
    #T[i+1]/MTBF0

    (act_beta[i+1] <- ppois(a[i], lambda = T[i+1]/MTBFA, lower.tail = FALSE))
    #(act_alpha[i+1] <- pchisq(
    #  MTBF0/MTBFA * qchisq(alpha, 2*(a[i]+1), lower.tail = FALSE),
    #  2*(a[i]+1)))

    if(act_beta[i] > beta){
      (a[i+1] <- a[i]+1)
      i <- i + 1
      #print(i)
    }
  }

  a <- a[-1]-1
  r <- a+1
  T <- T[-1]
  act_beta <- act_beta[-1]

  return(data.frame(Accept = a, Reject = r, Duration = T, beta = act_beta))
}





#' Calculates 2-sided CI for an exponential mean
#'
#' \code{exp_mean_ci} calculates the 2-sided CI
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), number of failures, confidence level, and assuming
#'   the test is time terminated.
#'
#' @param n A numeric vector indicating the observed number of failures.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param conf The desired level of confidence (values within 0.0:1.00
#'   default is set to 0.80).
#'
#' @return The output will be a numeric vector, indicating the LCB and UCB of
#'   the exponential mean with the given level of confidence. The units
#'   will be the same as the units of the supplied \code{duration}. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}
#'
#' @examples
#' # What is the 80% 2-sided CI for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 367 hours, and 0 failures.
#' exp_mean_ci(n = 0, duration = 367, conf = 0.80)
#'
#' # What is the 80% 2-sided CI for the MTBF (assuming the times between 
#'   # failure are exponentially distributed), given a test
#'   # time of 920 hours, and 7 failures.
#' exp_mean_ci(n = 7, duration = 920, conf = 0.80)
#'
#' @export
exp_mean_ci <- function(n, duration, conf = 0.8){

  if(conf >= 1 | conf <= 0){
    stop("conf must be between 0 and 1")
  }

  lwr_p <- (1-conf)/2
  chisq_lwr <- qchisq(p = lwr_p, df = 2 * n + 2, lower.tail = FALSE)
  chisq_upr <- qchisq(p = lwr_p, df = 2 * n, lower.tail = TRUE)
  lwr <- 2 * duration/chisq_lwr
  upr <- 2 * duration/chisq_upr
  ci <- c(lwr, upr)
  names(ci) <- c(lwr_p, 1-lwr_p)
  return(ci)
}
exp_mean_ci(n = 7, duration = 920, conf = 0.8)





#' Calcualtes the confidence that we've met the reliability requirement
#'
#' \code{rel_conf} calculates the confidence that we've met the reliability
#'   requirement.
#'
#' @param mtbf_req A numeric vector indicating the MTBF requirement.
#' @param duration A numeric vector indicating the tested duration
#'   (hours, miles, rounds, etc.).
#' @param r A numeric vector indicating the observed number of failures.
#'
#' @return The output will be a numeric vector, indicating the confidence
#'   that the MTBF requirement was met. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}, \code{\link{exp_mean_ci}},
#'   \code{\link{exp_mean_lcb}}
#'
#' @examples
#' # How much confidence do we have that the MTBF exceeds 250 hours, given
#'   # that we tested for 500 hours and observed 0 failures
#' rel_conf(250, 500, 0)
#'
#' @export
# Conf the prob we accept the null when the null is true
# Power the prob we reject the null when the null is false
# Our NULL hypothesis is:
# Ho: reliability < rel_req     or     mtbomf < mtbomf_req
# How much confidence do we have that we met the requirement?
#' @export
rel_conf <- function(mtbf_req, duration, r){
  pchisq( (2*duration)/mtbf_req, 2*(r+1), lower.tail = TRUE)
}