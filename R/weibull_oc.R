#' Weibull Operating Characteristic (OC) Curves
#'
#' \code{weibull_oc} computes OC curves for Weibull reliability tests
#'   (probability of passing test as a function of true prob or shape & scale)
#'
#' @param accept The allowable number of failures.
#' @param s The number of test units.
#' @param duration The test duration.
#' @param shape The Weibull shape parameter.
#' @param scale The Weibull scale parameter (could be left as NULL if R_m is supplied).
#'   This is \code{NULL} by default.
#' @param md The required mission duration for the reliability requirement.
#' @param R_m The reliability requirement for the given mission duration.
#'
#' @return Returns a data.frame with the probability of passing the test of a given
#'   duration with an allowable number of failures, given the true MTBF and R_m.
#' 
#' @examples
#' shape <- 2
#' scale_vec <- seq(4000, 6000, by = 2000)
#' mtbf_vec <- weibull_mean(shape, scale_vec)
#' weibull_oc(accept = 1, ss = 32, duration = 1500, shape = shape,
#'   scale = scale_vec, md = 2000, R_m = NULL, plot = F)
#'
#' weibull_oc(accept = 1, ss = 32, duration = 1500, shape = shape,
#'   scale = NULL, md = 2000, R_m = c(.7788008, 0.8948393), plot = F)
#'
#'
#' scale_vec <- seq(4000, 30000, by = 50)
#' scale <- weibull_scale(2000, R_m = .8, shape = shape)
#' mtbf <- weibull_mean(shape, scale)
#' p1 <- weibull_oc(accept = 1, ss = 32, duration = 1500, shape = shape,
#'   scale = scale_vec, md = 2000, R_m = NULL, plot = T)
#' p1$p + 
#'   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
#'   scale_x_continuous(limits = c(mtbf,10000), breaks = seq(3000,10000,1000))
#'
#' @export
weibull_oc <- function(accept, ss, duration, shape, scale = NULL, md, R_m = NULL, plot = T){

  call <- (match.call)

  if(!is.null(R_m)){
    if( any(R_m <= 0 | R_m >= 1) ){
      stop("R_m must be a probability between 0 and 1.")
    }
  }
  if( is.null(R_m) & is.null(scale) ){
    stop("Exactly one of R_m or scale must be set to NULL")
  }
  if( !is.null(R_m) & !is.null(scale) ){
    stop("Exactly one of R_m or scale must be set to NULL")
  }

  if(is.null(R_m)) {
    mtbf <- weibull_mean(shape, scale)
    R_m <- pweibull(md, shape, scale, lower.tail = FALSE)

    power <- pbinom(
      q = accept,
      size = ss,
  
      # prob of single unit failing demonstration
      prob = pweibull(duration, shape, scale, lower.tail = TRUE),
  
      # P(Accept <= accept) prob of <= accept unit(s) failing demonstration
      lower.tail = TRUE
    )

    df <- data.frame(power, mtbf, R_m)

    p <- ggplot(df, aes(x = mtbf, y = power)) +
      geom_line(size = 1, colour = "blue") +
      #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
      #scale_x_continuous(
      #  limits = c(mtbf,10000), breaks = seq(3000,10000,1000)
      #) +
      labs(
        x = "True MTBF",
        y = "Probability of Passing Demonstration"
      )
  }

  if(is.null(scale)) {
    scale <- weibull_scale(md, R_m, shape)
    mtbf <- weibull_mean(shape, scale)
    
    power <- pbinom(
      q = accept,
      size = ss,
  
      # prob of single unit failing demonstration
      prob = pweibull(duration, shape, scale, lower.tail = TRUE),
  
      # P(Accept <= accept) prob of <= accept unit(s) failing demonstration
      lower.tail = TRUE
    )

    df <- data.frame(power, mtbf, R_m)

    p <- ggplot(df, aes(x = R_m, y = power)) +
      geom_line(size = 1, colour = "blue") +
      #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
      #scale_x_continuous(
      #  limits = c(0.8,1), breaks = seq(.8, 1,.05)
      #) +
      labs(
        x = "True Prob of Unit Surviving Mission Duration",
        y = "Probability of Passing Demonstration"
      )
  }


  if(plot == TRUE) {return(list(p = print(p), df = df))}
  else{
    return(list(p = NULL, df = df))
  }
}



