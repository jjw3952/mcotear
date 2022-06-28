#' Nonparametric Estimate for the Rate of Occurrence of Failures (ROCOF)
#'
#' \code{rocof} calculates the sample nonparametric estimate of the
#'   intensity function (i.e. rate of occurrence of failures (ROCOF), or failure rate)
#'   at time t. The inverse of this would be the sample nonparametric
#'   mean time between failure at time t. This function is useful for the
#'   creation of Duane Plots as shown in the examples section below.
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param by If providing a list of length > 1 this can be
#'   a vector that defines a name for each element of the list
#'   so as to return by system rocof and mtbf estimates.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the 'by' variable (which specifies the system or process the failuresm
#'   are attributable to, if supplied in the list name), the failure times ("t"),
#'   and the corresponding nonparametric estimates for the instantaneous
#'   rocof and mtbf.
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_intensity}},
#'   \code{\link{power_law_mcf}}, \code{\link{mcf}}, \code{\link{trend_test}}, 
#'   \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#'
#' # Three systems failure times.
#' (df <- rocof(t = split(amsaa$Time, amsaa$System)))
#'
#' ggplot(df,
#'   aes(
#'     x = t,
#'     y = rocof)) +
#'   scale_x_log10() +
#'   scale_y_log10() +
#'   geom_smooth(method='lm', se = FALSE) +
#'   geom_point() +
#'   labs(y = "Cumulative Failure Rate") +
#'   scale_colour_manual(values = cbPalette) +
#'   ggtitle("Duane Plot")
#' 
#' ggplot(df,
#'   aes(
#'     x = t,
#'     y = mtbf)) +
#'   scale_x_log10() +
#'   scale_y_log10() +
#'   geom_smooth(method='lm', se = FALSE) +
#'   geom_point() +
#'   labs(y = "Cumulative MTBF") +
#'   scale_colour_manual(values = cbPalette) +
#'   ggtitle("Duane Plot")
#'
#'  rm(list = c("amsaa", "df"))
#'
#' @export
rocof <- function(t, by = NULL){

  if(is.null(by)){
    tsort <- sort(unlist(t))
    mtbf <- tsort/(1:length(tsort))
    rocof <- mtbf^-1
    df <- data.frame(t = tsort, mtbf = mtbf, rocof = rocof)
  } else{
  
  if(length(by) != length(t)) stop("length(by) != length(t)")

  t <- lapply(t, sort)

  n <- lapply(t, length)
  mtbf <- lapply(t, function(x) x/(1:length(x)))
  rocof <- lapply(mtbf, `^`, -1)

  # If the name is NULL then make it the list indice number
  names <- lapply(seq_along(t), function(i) ifelse(is.null(names(t)[[i]]), i, names(t)[[i]]))
  names(t) <- unlist(names)

  by <- rep(names(t), n)
  t <- unsplit(t, f = by)
  mtbf <- unsplit(mtbf, f = by)
  rocof <- unsplit(rocof, f = by)
  df <- data.frame(by = by, t = t, mtbf = mtbf, rocof = rocof)
  }

  return(df)
}