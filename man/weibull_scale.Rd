% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibull_scale.R
\name{weibull_scale}
\alias{weibull_scale}
\title{Calcualtes the Weibull Scale Paramater Based on the Shape and Reliability at a Given Time}
\usage{
weibull_scale(md, R_m, shape)
}
\arguments{
\item{md}{Mission Duration - The time at which Reliaiblity is defined.}

\item{R_m}{Mission Reliability - The Reliaiblity of a mission of a given duration \code{md}.}

\item{shale}{The Weibull shale parameter.}
}
\value{
The output will be a numeric value indicating the Weibull mean.
}
\description{
\code{weibull_scale} calculates the value of the Weibull scale parameter
  based on the shape parameter, and the reliability at a given time.
}
\examples{
# What is the scale parameter of a Weibull distribution with shape 2 in which there
  # is 80\% probability of surviving a mission of 2000 hours?
weibull_scale(2000, .8, 2)

}
\references{
Determining the Right Sample Size for Your Test: Theory and Application \href{https://www.weibull.com/pubs/2015_RAMS_right_sample_size.pdf}{2015 Annual Reliability and Maintainability Symposium}
}
\seealso{
\code{\link{weibull_mean}}
}
