#' Calcultes a SUS Score
#'
#' \code{sus_score} calcultes SUS scores given responses arranded
#'   in a data.frame or matrix.
#'
#' @param x A vector of SUS responses for a single record/observation.
#    There should be 10 responses (between 1 and 5), each representing 
#'   the response to a different SUS question.
#'
#' @return Returns a vector of SUS scores.
#'
#' @seealso \code{\link{sus_score_df}}, \code{\link{sus_to_percentile}},
#'   \code{\link{sus_suaro}}, \code{\link{gg_sus}}
#'
#' @examples
#' sus_score(x = rep(c(1,5), times = 5))
#' sus_score(x = rep(c(5,1), times = 5))

#' 
#' @export
sus_score <- function(x){
  if(length(x) != 10){
    stop("Input variable 'x' should be a numeric vector of length 10.")
  }
  if(min(x) < 0 | max(x) > 5){
    stop("All elements of input variable 'x' should be within the range 0:5.")
  }

  # where 4 is the difference between the min and max response
  10/4 * ( sum(x[seq(1,9,2)]-1) + sum(5-x[seq(2,10,2)]) )
}