#' Exponential Fixed Duration Test Plans
#'
#' \code{fixed_duration_tests} See pg. 67 of MIL-HDBK-781A. Fixed duration
#'   tests for exponentially distributed MTBF with desired alpha and beta.
#'
#'
#' @param MTBF0 The MTBF below which to reject the system with probability alpha.
#' @param MTBFA The MTBF above which to accept the system with probability 1-beta.
#' @param alpha The type I error rate, producer's risk - the probability we reject
#'   a system with MTBF > MTBFA.
#' @param beta The type II error rate, consumer's risk - the probability we accept
#'   a system with MTBF < MTBF0.
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
#' @references
#' Mil-Hdbk-781A
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
