#' Probability Ratio Sequential Test (PRST)
#'
#' \code{binom_prst} returns parameters defining a PRST for the binomial distribution as developed by Abraham Wald.
#'
#' @param p0 The minimum acceptable probability of failure.
#' @param p1 The maximum acceptable probability of failure. This value must be greater than \code{p0}.
#' @param m An integer indicating the maximum number of units allowable for test.
#' @param alpha The producer's risk of rejecting equipment with probability of failure p < \code{p0}
#'   (the probility of rejecting good equipment). Default = 0.10.
#' @param beta The consumer's risk of accepting equipment with probability of failure p > \code{p1}.
#'   (the probility of accepting bad equipment). Default = 0.10.
#' @param r (Optional, default as NULL) The cumulative number of failures up to the number of units tested, \code{t}.
#'   If specified it must be of equal length to t.
#' @param t (Optional, default as NULL) The cumulative number of units tested.
#'   If specified it must be of equal length to \code{r}.
#'
#' @return The output will return a table indicating when to accept
#' or reject the equipment based on the number of units tested and
#' failures observed. The binomial prst plot will be printed, along
#' with the Operating Characteristic Curve and Average Sample Number plot.
#'
#' @seealso \code{\link{exp_prst}}, \code{\link{exp_prst_plot}}
#'
#' @references
#' Wald, Abraham. Sequential Analysis. John Wiley & Sons, 1947.
#'
#' @examples
#' binom_prst(p0 = 0.05, p1 = 0.10, m=300, alpha=0.10, beta=0.10)
#' binom_prst(p0 = 0.05, p1 = 0.10, m=300, alpha=0.10, beta=0.20)
#' binom_prst(p0 = 0.05, p1 = 0.10, m=300, alpha=0.05, beta=0.20)
#'
#' @export
binom_prst <- function(p0 = p0, p1 = p1, m = m, alpha = 0.10, beta = 0.10, r = NULL, t = NULL){
  
  if(p0 >= p1){
    stop("p1 must be > p0")
  }
  if(alpha >= 1 | alpha <= 0 | beta >= 1 | beta <= 0 | p0 >= 1 | p0 <= 0 | p1 >= 1 | p1 <= 0){
    stop("alpha, beta, p0, and p1 must all be between 0 and 1")
  }
  
  m <- 1:ceiling(max(m))
  logratio1 <- log((1-p1)/(1-p0))
  logratio2 <- log(p1/p0)
  logratio3 <- -logratio1
  diff21 <- logratio2-logratio1
  B <- beta/(1-alpha)
  A <- (1-beta)/alpha
  
  (slope <- (logratio3/(diff21)))
  intercept0 <- log(B)/diff21
  intercept1 <- log(A)/diff21
  
  # acceptance number
  am <- intercept0 + m*slope

  # rejection number
  rm <- intercept1 + m*slope

  # accept/reject data.table  
  df_ar <- data.frame(
    units.tested = m,
    accept.le.x.defects = floor(am),
    reject.ge.x.defects = ceiling(rm)
  )
  setDT(df_ar)
  df_ar[reject.ge.x.defects > units.tested, reject.ge.x.defects := NA]
  df_ar[accept.le.x.defects < 0, accept.le.x.defects := NA]
  #df_ar
  
  
  names(df_ar)[2:3] <- c("Accept", "Reject")
  df_ar_long <- melt.data.table(
    df_ar, id.vars = "units.tested",
    variable.name = "Decision", value.name = "defectives")
  
  x <- seq(-intercept0/slope, max(m), length.out = 100)
  accept <- intercept0 + slope * x
  df_accept_ribbon <- data.frame(x = x, accept = accept)
  
  x <- seq(0, max(m), length.out = 100)
  reject <- intercept1 + slope * x
  df_reject_ribbon <- data.frame(x = x, reject = reject)
  
  fail_dat <- data.frame(r, t)
  
  prst_plot <- ggplot(df_ar_long) +
    geom_line(
      aes(x = units.tested, y = defectives, 
        colour = Decision, group = Decision), colour = "transparent") +
    geom_abline(aes(intercept = intercept0, slope = slope), colour = "green2") +
    geom_abline(aes(intercept = intercept1, slope = slope), colour = "red") +
    scale_colour_manual("Decision", 
        values = c(Accept = "green2", Reject = "red")) + 
    scale_y_continuous(breaks = seq(0, max(df_ar_long$defectives, na.rm = TRUE))) + 
    labs(x = "Units Tested", y = "Failures") +
    theme(
      panel.grid.minor.y = element_blank()
    ) +
    geom_ribbon(data = df_accept_ribbon, 
      mapping = aes(x = x, ymin = 0, ymax = accept), fill = "green2", 
        alpha = 0.2) +
    geom_ribbon(data = df_reject_ribbon,
      mapping = aes(x = x, ymin = reject, ymax = Inf), fill = "red", 
        alpha = 0.2) +
    geom_step(
      data = fail_dat,
      aes(x = t, y = r)
    ) +
    geom_point(
      data = fail_dat,
      aes(x = t, y = r #, fill = I(color)
          ), shape = 21)
  # print(prst_plot)
  
  # Operating Characteristic Function
  h <- seq(-100, 100, length = 1000)
  l_p <- ((A^h)-1)/((A^h) - (B^h))
  #range(l_p)
  p <- (1-((1-p1)/(1-p0))^h)/((p1/p0)^h - ((1-p1)/(1-p0))^h)
  #range(p)
  
  l_p_df <- data.table(p = p, l_p = l_p) 
  
  oc_plot <- l_p_df %>%
    ggplot() +
    geom_line(aes(x = p, y = l_p)) +
    labs(
      x = "Probability of Defect",
      y = "Probability of Passing (Acceptance)"
    ) +
    scale_x_continuous(breaks = seq(0.0, 1.0, .1), limits = c(0.0,1.0)) +
    scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0,1)) +
     theme(
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      data = l_p_df[which.min(abs(p-p0))],
      aes(x = p, y = l_p)
    ) +
    geom_text(
      data = l_p_df[which.min(abs(p-p0))],
      aes(x = p, y = l_p, label = paste0(p0, ", ", round(l_p,2), " (1-alpha)")),
      size = 2.5, hjust = -.10
    ) +
    geom_point(
      data = l_p_df[which.min(abs(p-p1))],
      aes(x = p, y = l_p)
    ) +
    geom_text(
      data = l_p_df[which.min(abs(p-p1))],
      aes(x = p, y = l_p, label = paste0(p1, ", ", round(l_p,2), " (beta)")),
      size = 2.5, hjust = -.10
    )
  # print(oc_plot)
  
  # Average Sample Number (ASN)
    ep_n <- (l_p*log(B) + (1-l_p)*log(A)) / (p*logratio2 + (1-p)*logratio1)

  # This is to help identify the max y-axis value
  maxn <- ceiling(max(ep_n))
  len <- nchar(maxn)
  new_num <- as.numeric(substr(round(maxn, digits = 2), 1 , len-1))
  
  remainder <- new_num %% 5^(len-2)
  (new_num <- new_num + (5^(len-3)) * (5-remainder))
  (maxn <- new_num*10)
     
  asn_df <- data.table(p = p, ep_n = ep_n)

  asn_plot <- asn_df %>%
    ggplot() +
    geom_line(aes(x = p, y = ep_n)) +
    labs(
      x = "Probability of Defect",
      y = "Average Sample Number"
    ) +
    scale_x_continuous(breaks = seq(0.0, 1.0, .1), limits = c(0.0,1.0)) +
    scale_y_continuous(breaks = seq(0, maxn, 10), limits = c(0, maxn)) +
    theme(
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      data = asn_df[which.min(abs(p-p0))],
      aes(x = p, y = ep_n)
    ) +
    geom_text(
      data = asn_df[which.min(abs(p-p0))],
      aes(x = p, y = ep_n, label = paste0(p0, ", ", ceiling(ep_n))),
      size = 2.5, hjust = 1.20
    ) +
    geom_point(
      data = asn_df[which.min(abs(p-p1))],
      aes(x = p, y = ep_n)
    ) +
    geom_text(
      data = asn_df[which.min(abs(p-p1))],
      aes(x = p, y = ep_n, label = paste0(p1, ", ", ceiling(ep_n))),
      size = 2.5, hjust = -.5
    )
  # print(asn_plot)
  
  return(list(table = df_ar, plot = prst_plot, oc = oc_plot, asn = asn_plot))
}