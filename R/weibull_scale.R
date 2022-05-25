#' Calcualtes the Weibull Scale Paramater Based on the Shape and Reliability at a Given Time
#'
#' \code{weibull_scale} calculates the value of the Weibull scale parameter
#'   based on the shape parameter, and the reliability at a given time.
#'
#' @param md Mission Duration - The time at which Reliaiblity is defined.
#' @param R_m Mission Reliability - The Reliaiblity of a mission of a given duration \code{md}.
#' @param shale The Weibull shale parameter.
#'
#' @return The output will be a numeric value indicating the Weibull mean. 
#'
#' @seealso \code{\link{weibull_mean}}
#'
#' @references
#' Determining the Right Sample Size for Your Test: Theory and Application \href{https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwi79beh0vr3AhXhrHIEHQ0TDp8QFnoECAUQAQ&url=https%3A%2F%2Fwww.weibull.com%2Fpubs%2F2013_RAMS_determining_right_sample_size.pdf&usg=AOvVaw13YxB76WJjjzoFbfCb_5z0}{2015 Annual Reliability and Maintainability Symposium}
#'
#' @examples
#' # What is the scale parameter of a Weibull distribution with shape 2 in which there
#'   is 80% probability of surviving a mission of 2000 hours?
#' weibull_scale(2000, .8, 2)
#'
#' @export
weibull_scale <- function(md, R_m, shape){

  if(R_m <= 0 | R_m >= 1){
    stop("R_m must be a probability between 0 and 1.")
  }

  md / (-log(R_m))^(1/shape)
}
