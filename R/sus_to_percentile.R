#' Converts a System Usability Scale score to a corresponding percentile rank
#'
#' \code{sus_to_percentile} converts a valid SUS score (0:100), into
#'   the corresponding percentile rank (0.00:1.00).
#'
#' @param x A valid SUS score ranging within the range 0:100.
#'
#' @return Returns a \code{\link[base]{data.frame}} of x- (SUS scores),
#'   and y-values (percentiles).
#'
#' @seealso \code{\link{gg_sus}}, \code{\link{sus_suaro}}
#'
#' @examples
#' sus_to_percentile(c(68, 90))
#' @export
sus_to_percentile <- function(x){
  df <- structure(
    list(
      score = c(0L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 
        40L, 45L, 50L, 55L, 60L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 
        73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L, 85L, 90L, 95L, 100L),
 
      percentile = c(0, 0.003, 0.004, 0.007, 0.01, 0.015, 0.02, 
        0.04, 0.06, 0.08, 0.13, 0.19, 0.29, 0.41, 0.44, 0.47, 0.5, 
        0.53, 0.56, 0.6, 0.63, 0.67, 0.7, 0.73, 0.77, 0.8, 0.83, 
        0.86, 0.88, 0.97, 0.998, 0.99999, 1)),

     .Names = c("score", "percentile"),
     class = "data.frame", row.names = c(NA, -33L))

  model <- smooth.spline(
    x = df$score,
    y = df$percentile,
    spar = 100^-1000, all.knots = T, tol = 10^-1)

  return( data.frame(predict(model, x)))
}