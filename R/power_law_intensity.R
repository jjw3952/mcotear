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
#' @seealso \code{\link{power_law_process}}, \code{\link{mcf}}, \code{\link{rocof}},
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
#' df_mcf <- mcf(t = split(amsaa$Time, list(200,200,200), amsaa$System))
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
  # Expected Failure Rate at Time t

  t <- sort(unlist(t, use.names = FALSE))
  intensity <- (beta/lambda)*(t/lambda)^(beta-1)

  return( data.frame(t = t, power_intensity = intensity) )
}