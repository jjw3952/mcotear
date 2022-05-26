#' Calculate the Required Test Duration or Sample Size
#'
#' \code{weibull_rel_demo} calculates the required test duration or sample size
#'   (number of units to be tested) based on the alpha value, Weibull shape parameter,
#'   mission duration requirement and corresponding reliability requirement. Leave one of
#'   \code{t} or \code{ss} set to \code{NULL} to solve for it.
#'
#' @param t The test duration. Set to \code{NULL} to solve for test duration. Default is \code{NULL}.
#' @param ss The sample size (number of units) to be used in the test.
#'   Set to \code{NULL} to solve for sample size. Default is 1. If set it must be > \code{r}.
#' @param r The allowable number of failures in the test. Default is 0.
#' @param alpha The alpha value (Type I Error Rate) to be used in the test. Default is 0.20.
#' @param shape The Weibull shape parameter. Default is 1 corresponding to an exponential distribution.
#'   Shape > 1 indicates increasing failure rate, Shape < 1 indicates decreasing failure rate.
#' @param md The mission duration requirement.
#' @param R_m The reliability requirement for the specified mission duration.
#' @param lower The lower bound in the search range (the lower of where to look for t or ss).
#'   Required for the \code{\link[stats]{uniroot}} or \code{\link[ssanv]{uniroot.integer}} functions.
#' @param upper The upper bound in the search range (the upper of where to look for t or ss).
#'   Required for the \code{\link[stats]{uniroot}} or \code{\link[ssanv]{uniroot.integer}} functions.
#' @param ... Additional options parameter to be passed to the \code{\link[stats]{uniroot}} or
#'   \code{\link[ssanv]{uniroot.integer}}functions.
#'
#' @return The output will be from the \code{\link[stats]{uniroot}} or \code{\link[ssanv]{uniroot.integer}} functions.
#'   The root element of the return (as in \code{weibull_rel_demo(...)$root}) provides the solution.
#'
#' @seealso \code{\link{weibull_scale}}, \code{\link{weibull_mean}}
#'
#' @references
#' Determining the Right Sample Size for Your Test: Theory and Application \href{https://www.weibull.com/pubs/2015_RAMS_right_sample_size.pdf}{2015 Annual Reliability and Maintainability Symposium}
#'
#' @examples
#' # What is the required sample size given 1 allowable failure
#'   # in 1500 hours to demonstrate with 90% confidence that
#'   # a system has 0.80 probability of surviving to 2000 hours
#'   # without failure?
#' weibull_rel_demo(t = 1500, ss = NULL, r = 1, alpha = .1,
#'   shape = 2, md = 2000, R_m = .8, lower=1, upper = 100)
#'
#' # What is the required test duration (per system), if I have 10 systems,
#'   # allow for a total of 1 failure, in order to demonstrate with 90%
#'   # confidence that a system has 0.80 probability of surviving to 2000
#'   # hours without failure?
#' weibull_rel_demo(t = NULL, ss = 10, r = 1, alpha = .1,
#'   shape = 2, md = 2000, R_m = .8, lower=1, upper = 10000)
#' 
#' # These are equivalent since shape = 1 reduces to exponential
#' # However, if the number of allowable failures (r) increases, the ss must increase and
#'   # the return from the exp_test_duration function will not match
#' weibull_rel_demo(t = NULL, ss = 1, r = 0, alpha = .1,
#'   shape = 1, md = 2000, R_m = .8, lower=1, upper = 100000)
#' mtbf_req(.8, 2000)
#' exp_test_duration(r = 0, mtbf = 8962.84, conf = .9)
#'
#' @export
weibull_rel_demo <- function(t, ss, r, alpha, shape, md, R_m, lower, upper, ...){

  scale <- md / (-log(R_m))^(1/shape)

  if(is.null(t) & is.null(ss)){
    stop(
      paste(
        "exactly one of t or ss must be NULL",
        "all other parameters must be numeric", sep = "\n"
      )
    )
  }

  if( !is.null(ss) ){
    if(r >= ss) stop("r must be > ss")
  }

  if(is.null(t)){
    opt_fun <- function(x, r, ss, shape, scale, alpha){
      pbinom(
        q = r, size = ss,
        prob = pweibull(x, shape, scale, lower.tail = TRUE),
        lower.tail = TRUE)-alpha
    }

    return(uniroot(
      opt_fun, lower=lower, upper=upper, r=r, ss=ss,
      shape=shape, scale=scale, alpha=alpha, ...
    ))
  }

  if(is.null(ss)){
    opt_fun <- function(x, r, t, shape, scale, alpha){
      pbinom(
        q = r, size = x,
        prob = pweibull(t, shape, scale, lower.tail = TRUE),
        lower.tail = TRUE)-alpha
    }

    return(ssanv::uniroot.integer(
      opt_fun, lower=lower, upper=upper, r=r, t=t,
      shape=shape , scale=scale, alpha=alpha, ...
    ))
  }
}