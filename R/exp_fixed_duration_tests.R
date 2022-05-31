#' Exponential Fixed Duration Test Plans
#'
#' \code{exp_fixed_duration_tests} See pg. 67 of MIL-HDBK-781A. Fixed duration
#'   tests for exponentially distributed MTBF with desired alpha and beta.
#'
#'
#' @param mtbf0 The MTBF below which to reject the system with probability alpha.
#' @param mtbfA The MTBF above which to accept the system with probability 1-beta.
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
#' @seealso \code{\link{exp_mtbf_req}}, \code{\link{exp_reliability_req}},
#'   \code{\link{exp_test_duration}}, \code{\link{exp_mean_lcb}},
#'   \code{\link{exp_mean_ci}}, \code{\link{exp_oc}}
#'
#' @references
#' Mil-Hdbk-781A
#'
#' @examples
#' (fd <- exp_fixed_duration_tests(mtbf0 = 500, mtbfa = 1000, alpha = .2, beta = .2))
#' theta <- seq(250, 2000, 50)
#' prob_pass <- exp_oc(accept = fd$Accept[6], duration = fd$Duration[6], mtbf = theta)
#'
#' df <- data.frame(x = c(500,1000), y = c(0.20, 1-fd$beta[6]), label = c("alpha", "1-beta"))
#'
#' ggplot(data.frame(prob_pass = prob_pass, theta = theta)) +
#'   geom_point(ggplot2::aes(x = theta, y = prob_pass)) +
#'   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   geom_point(
#'     data = df,
#'     mapping = aes(x, y), colour = "red"
#'   ) +
#'   geom_text(
#'     data = df,
#'     mapping = aes(x, y, label = label), parse = TRUE, colour = "red", hjust = c(-1,1.2)
#'   )
#'
#' # Recipe for plotting a single OC Curve
#' alpha <- .2
#' beta <- .2
#' mtbf0 <- 500
#' mtbfa <- 1000
#' (fd <- exp_fixed_duration_tests(mtbf0 = mtbf0, mtbfa = mtbfa, alpha = alpha, beta = beta))
#' test <- 6 # select row of test from fd
#' theta <- seq(250, 2000, 50)
#' prob_pass <- exp_oc(accept = fd$Accept[test], duration = fd$Duration[test], mtbf = theta)
#' 
#' df <- data.frame(x = c(mtbf0, mtbfa), y = c(alpha, 1-fd$beta[test]), label = c("alpha", "1-beta"))
#' 
#' ggplot(data.frame(prob_pass = prob_pass, theta = theta)) +
#'   geom_point(aes(x = theta, y = prob_pass)) +
#'   geom_path(aes(x = theta, y = prob_pass)) +
#'   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   geom_point(
#'     data = df,
#'     mapping = aes(x, y), colour = "red"
#'   ) +
#'   geom_text(
#'     data = df,
#'     mapping = aes(x, y, label = label), parse = TRUE, colour = "red", hjust = c(-1,1.2)
#'   ) +
#'   labs(
#'     x = "MTBF", y = "Prob of Acceptance",
#'     title = "Exponential OC Curve",
#'     subtitle = paste0("Test Duration = ", ceiling(fd$Duration[test]), ". Allowable Failures = ", fd$Accept[test])
#'   )
#' 
#' # Recipe for plotting multiple OC curves
#' alpha <- .2
#' beta <- .2
#' mtbf0 <- 500
#' mtbfa <- 1000
#' (fd <- exp_fixed_duration_tests(mtbf0 = mtbf0, mtbfa = mtbfa, alpha = alpha, beta = beta))
#' test <- 1:7 # select row(s) of test from fd
#' theta <- seq(mtbf0/2, mtbf0*8.8, 50)

#' #library(purrr)
#' #library(reshape2)
#' prob_pass <- purrr::map2(
#'   .x = fd$Accept[test],
#'   .y = fd$Duration[test],
#'   .f = ~exp_oc(accept = .x, duration = .y, mtbf = theta))
#' names(prob_pass) <- paste0("OC", 1:length(prob_pass))
#' prob_pass <- as.data.frame(prob_pass)
#' prob_pass <- reshape2::melt(prob_pass, id.vars = NULL, value.name = "prob_pass", variable.name = "OC")
#' prob_pass$Accept <- rep(ceiling(fd$Accept[test]), each = length(theta))
#' prob_pass$Duration <- rep(ceiling(fd$Duration[test]), each = length(theta))
#' prob_pass$MTBF <- theta
#' head(prob_pass)
#' 
#' 
#' df <- unique(data.frame(
#'   x = rep(c(MTBF0,MTBFA), each = length(test)),
#'   y = c(rep(alpha, length(test)), 1-fd$beta[test]),
#'   label = rep(c("alpha", "1-beta"), each = length(test))
#'   ))
#' 
#' exp_test_duration(0, 500)
#' 
#' ggplot(
#'   prob_pass,
#'   aes(
#'     x = MTBF,
#'     y = prob_pass,
#'     colour = interaction(Accept, Duration, sep = ", ")
#'     )
#'   ) +
#'   geom_hline(yintercept = 1-beta, colour = "red", linetype = "longdash") +
#'   geom_point(size = .75) +
#'   geom_path() +
#'   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   geom_point(
#'     inherit.aes = FALSE,
#'     data = df,
#'     mapping = aes(x, y, fill = label), shape = 21, colour = "transparent"
#'   ) +
#'   scale_fill_manual(
#'     "Measure of Merit",
#'     values = c("alpha" = "blue", "1-beta" = "red"),
#'     labels = scales::parse_format()
#'   ) +
#'   labs(
#'     colour = "Allowable Failures,\nTest Duration",
#'     x = "MTBF", y = "Prob of Acceptance",
#'     title = "Exponential OC Curve",
#'     subtitle = bquote(atop(H[0]*": MTBF"<=.(MTBF0)*phantom(0),H[a]*": MTBF">.(MTBFA)))
#'   ) +
#'   guides(
#'     fill = guide_legend(
#'       override.aes = list(
#'         colour = "transparent" #c("blue", "red")
#'         )
#'       )
#'   ) +
#'   theme(
#'     # plot.title = element_text(vjust = 1, lineheight = 1, margin = margin(0,0,-35.5,0)),
#'     # plot.subtitle = element_text(hjust = 1, vjust = 0, lineheight = 1, margin = margin(0,0,5.5,0) ),
#'     # plot.margin = margin(30,0,0,0)
#'   )
#' # getwd()
#' # ggsave("Exp OC.png", height = 5, width = 6.5)
#' # grid::grid.ls(grid::grid.force())
#' # grid::grid.gedit("key-3-1-1.4-2-4-2", size = grid::unit(7, "points"))
#' # grid::grid.gedit("key-4-1-1.5-2-5-2", size = grid::unit(7, "points"))
#'
#' @export
exp_fixed_duration_tests <- function(mtbf0, mtbfa, alpha = .2, beta = .2){

  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0){
    stop("alpha and beta must both be between 0 and 1")
  }

  a <- 0
  i <- 1
  T <- 0
  act_beta <- 1
  while(act_beta[i] > beta){
    #fn <- function(x) abs(beta -  ppois(a, lambda = x/mtbf0, lower.tail = TRUE))
    #(T <- optimize(fn, c(0,upr), tol = 1E-4)$min)

    #fn1 <- function(x) {alpha -  ppois(a[i], lambda = x/mtbf0, lower.tail = TRUE)}
    #(T[i+1] <- uniroot(fn1, c(0,upr), tol = 1E-4)$root)
    (T[i+1] <- exp_test_duration(a[i], mtbf0, conf = 1-alpha)[[2]])
    #T[i+1]/MTBF0

    (act_beta[i+1] <- ppois(a[i], lambda = T[i+1]/mtbfa, lower.tail = FALSE))
    #(act_alpha[i+1] <- pchisq(
    #  mtbf0/mtbfa * qchisq(alpha, 2*(a[i]+1), lower.tail = FALSE),
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
