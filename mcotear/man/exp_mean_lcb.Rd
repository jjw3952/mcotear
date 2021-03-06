% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliability.R
\name{exp_mean_lcb}
\alias{exp_mean_lcb}
\title{Calculates the lower confidence bound for an exponential mean}
\usage{
exp_mean_lcb(n, duration, conf = 0.8)
}
\arguments{
\item{n}{A numeric vector indicating the observed number of failures.}

\item{duration}{A numeric vector indicating the tested duration
(hours, miles, rounds, etc.).}

\item{conf}{The desired level of confidence (values within 0.0:1.00
default is set to 0.80).}
}
\value{
The output will be a numeric vector, indicating the LCB of
  the exponential mean with the given level of confidence. The units
  will be the same as the units of the supplied \code{duration}.
}
\description{
\code{exp_mean_lcb} calculates the lower confidence bound (LCB)
  for an exponential mean given a test duration (hours, miles,
  rounds, etc.), confidence level, and assuming the test is
  time terminated.
}
\examples{
# What is the 80\% LCB for the MTBF (assuming the times between 
  # failure are exponentially distributed), given a test
  # time of 367 hours, and 0 failures.
exp_mean_lcb(n = 0, duration = 367, conf = 0.80)

}
\seealso{
\code{\link{mtbf_req}}, \code{\link{reliability_req}},
  \code{\link{test_duration}}, \code{\link{test_demo}},
  \code{\link{exp_equal_mtbf}}
}
