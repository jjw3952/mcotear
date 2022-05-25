#' Nonparametric Estimate for the Mean Cumulative Function
#'
#' \code{mcf} calculates the sample nonparametric estimate of the mean
#'   cumulative function (expected number of failures at time t).
#'
#' @param t A list of failure time vectors. Each vector should indicate
#'   a different system, i.e. if you have multiple systems each
#'   systems' failure times should be in it's own vector.
#' @param T A list of Total Time on Test (TTT) (i.e. test duration) vectors.
#'   The vectors in the list should be of length 1, and each vector should
#'   indicate a different system, i.e. if you have multiple systems each
#'   systems' TTT should be in it's own vector.
#' @param by If providing a list of length > 1 this can be
#'   a vector that defines a name for each element of the list
#'   so as to return by system mcf estimates.
#'
#' @return The output will be a \code{\link[base]{data.frame}} containing,
#'   the ordered failure times ("t") and corresponding nonparametric
#'    mcf estimate ("mcf").
#'
#' @seealso \code{\link{power_law_process}}, \code{\link{power_law_mcf}},
#'   \code{\link{trend_test}}, \code{\link{ttt}}, \code{\link{common_beta}}
#'
#' @examples
#' data(amsaa)
#' 
#' mcf(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   by = NULL)
#' 
#' mcf(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(200,200,200),
#'   by = names(split(amsaa$Time, amsaa$System)))
#' 
#' mcf(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(197.2,190.8,195.8),
#'   by = names(split(amsaa$Time, amsaa$System)))
#' 
#' mcf(
#'   t = split(amsaa$Time, amsaa$System),
#'   T = list(197.2,190.8,195.8),
#'   by = NULL)
#' 
#' @export
mcf <- function(t, T, by = NULL){

  if(is.null(by)){

  (k <- length(t))         # number of systems
  t <- unlist(t[!is.na(t)], use.names = F)
  (n <- length(t[!is.na(t)]))         # number of failures

  # Find position of failure times that occur more than once
  #  dups <- rev(which(duplicated(t[order(t)]) == TRUE))
  #  dups

  t.sort <- sort(unique(t))
  #i <- 1
  mcf <- NULL
  for(i in seq_along(t.sort)){
    mcf[i] <- length(which(t == t.sort[i])) / length(which(T >= t.sort[i]))
  }
  mcf
  
  df <- data.frame(t = t.sort, mcf = cumsum(mcf))

  #mcf <- seq(n/k/n, n/k, length.out = n)
  
  #for(i in seq_along(dups)){
  #  mcf[dups[i]-1] <- mcf[dups[i]]
  #}
  #df <- data.frame(t = sort(t), mcf)


  } else{
  
  (k <- length(t))           # number of systems
  out <- rep(list(NULL), k)

  for(i in seq_along(1:k)){
    ti <- t[[i]]
    (n <- length(ti))         # number of failures

    # Find position of failure times that occur more than once
    dups <- rev(which(duplicated(ti[order(ti)]) == TRUE))
    mcf <- 1:n
  
    for(j in seq_along(dups)){
      mcf[dups[j]-1] <- mcf[dups[j]]
    }

    out[[i]] <- mcf
  }

  df <- data.frame(by = rep(by, lapply(out, length)), t = unlist(t), mcf = unlist(out))
  df$mcf[which(is.na(df$t))] <- 0

  }

  return( df )
}