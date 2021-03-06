% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliability.R
\name{exp_equal_mtbf}
\alias{exp_equal_mtbf}
\title{Compare MTBFs for Exponential Distributions}
\usage{
exp_equal_mtbf(T, r)
}
\arguments{
\item{T}{A vector with test times, with the same length as r.}

\item{r}{A vector of failures, with the same length at T.}
}
\value{
The output is a list with the p-value and interpretation help.
}
\description{
\code{exp_equal_mtbf} campares MTBF parameters from exponential distributions
  and returns the p-value.
}
\examples{
  # from pg 189-192 of Statistical Methods for the Reliability of
   # Repairable Systems; Rigon and Basu
  exp_equal_mtbf(T = c(152, 122), r = c(13, 7))
  exp_equal_mtbf(T = c(152, 122), r = c(13))
  exp_equal_mtbf(T = c(493, 687, 456, 722), r = c(6, 6, 6, 6))

}
\seealso{
\code{\link{mtbf_req}}, \code{\link{reliability_req}},
  \code{\link{test_duration}}, \code{\link{exp_mean_lcb}}
  \code{\link{test_demo}}
}
