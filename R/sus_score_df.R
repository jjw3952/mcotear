#' Calculte SUS Scores
#'
#' \code{sus_score_df} calcultes SUS scores given responses arranded
#'   in a data.frame or matrix.
#'
#' @param df A data.frame or matrix of SUS scores. Each row should be a
#'   different record/observation. There should be 10 columns,
#'   each representing the response to a different SUS question.
#'
#' @return Returns a vector of SUS scores.
#'
#' @seealso \code{\link{sus_score}}, \code{\link{sus_to_percentile}},
#'   \code{\link{sus_suaro}}, \code{\link{gg_sus}}
#'
#' @examples
#' df <- data.frame(
#'   x1 = c(1,5), x2 = c(5,1), x3 = c(1,5), x4 = c(5,1),
#'   x5 = c(1,5), x6 = c(5,1), x7 = c(1,5), x8 = c(5,1),
#'   x9 = c(1,5), x10 = c(5,1)
#' )
#' sus_score_df(df)
#' rm(list = "df")
#' 
#' @export
sus_score_df <- function(df){
  if(ncol(df) != 10){
    stop("Input variable 'df' should be a data.frame or matrix with 10 columns.")
  }
  apply(X = df, MARGIN = 1, FUN = sus_score)
}