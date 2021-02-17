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
#'   with one column as the number of failures, 
#'   and the second column defining the required test
#'   duration (in the same units as the \code{mtbf} parameter), needed
#'   to satisfy the MTBF requirement with the given level of confidence,
#'   and the allowable number of failures.
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{exp_mean_lcb}}, \code{\link{test_demo}},
#'   \code{\link{exp_equal_mtbf}}
#'
#' @examples
#' # What is the required test duration to demonstrate the MTBF is at least
#'   # 228 hours, given 0:5 failures, and 80% confidence?
#' test_duration(r = 0:5, mtbf = 228, conf = 0.80)
#'
#' @export
test_duration <- function(r, mtbf, conf = 0.80){
  chisq <- qchisq(p = conf, df = 2*r+2, lower.tail = TRUE)
  duration <- mtbf*chisq/2
  return( data.frame(Failures = r, Duration = duration) )
}





#' Calculates the lower confidence bound for an exponential mean
#'
#' \code{exp_mean_lcb} calculates the lower confidence bound (LCB)
#'   for an exponential mean given a test duration (hours, miles,
#'   rounds, etc.), confidence level, and assuming the test is
#'   time terminated.
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
#'   allowable number of failures (r), and true type II error rate (Beta)
#'   (type II error rate = probability the null hypothesis is not rejected
#'   when it is false). 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{rel_power}}, \code{\link{exp_equal_mtbf}}
#'
#' @examples
#' # What is the required test length and allowable number of failures,  
#'   # to demonstrate MTBF > 180, with 80% power and confidence, and allowing 
#'   # one failure (and assuming the times between failure are
#'   # exponentially distributed)?
#' test_demo(mtbf0 = 180, mtbf1 = 300, alpha = 0.2, beta = 0.2)
#'
#' @export
test_demo <- function(mtbf0, mtbf1, alpha = 0.2, beta = 0.2){
  r <- 0
  while(
    qchisq(beta, 2*(r+1))/qchisq(alpha, 2*(r+1), lower.tail = FALSE) < (mtbf0/mtbf1)
  ) { r <- r + 1 }
 
  T <- mtbf0 * .5 * qchisq(alpha, 2*(r+1), lower.tail = FALSE)
  Beta <- pchisq(
    mtbf0/mtbf1 * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1))
  list(T=T,r=r,Beta=Beta)
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
#'   within 0.0:1.00, default is set to 0.20).
#'
#' @return The output is a numeric value representing the power of rejecting
#'   the null hypothesis. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}}
#'   \code{\link{test_demo}}, \code{\link{exp_equal_mtbf}}
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
rel_power <- function(mtbf0, mtbf1, r = 1, alpha = 0.2){
  pchisq(
    (mtbf0/mtbf1) * qchisq(alpha, 2*(r+1), lower.tail = FALSE),
    2*(r+1), lower.tail = FALSE
  )
}





#' Compare MTBFs for Exponential Distributions
#'
#' \code{exp_equal_mtbf} campares MTBF parameters from exponential distributions
#'   and returns the p-value.
#'
#' @param T A vector with test times, with the same length as r.
#' @param r A vector of failures, with the same length at T.
#'
#' @return The output is a list with the p-value and interpretation help. 
#'
#' @seealso \code{\link{mtbf_req}}, \code{\link{reliability_req}},
#'   \code{\link{test_duration}}, \code{\link{exp_mean_lcb}}
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