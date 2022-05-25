#' Mean of a Weibull Distribution given known Parameters
#'
#' \code{weibull_mean} the mean for a Weibull distribution based on
#'   the shape and scale parameters.
#'
#' @param shape The Weibull shape parameter.
#' @param scale The Weibull scale parameter.
#'
#' @return The output will be a numeric value indicating the Weibull mean. 
#'
#' @seealso \code{\link{weibull_scale}}
#'
#' @references
#' /url{https://en.wikipedia.org/wiki/Weibull_distribution}
#'
#' @examples
#' # What is the mean of a Weibull distribution with shape 2 and scale 2000?
#' weibull_mean(2, 2000)
#' # What is the mean of a Weibull distribution with shape 1 and scale 2000?
#' weibull_mean(1, 2000)
#'
#' @export
weibull_mean <- function(shape, scale){
  scale*gamma(1+1/shape)
}