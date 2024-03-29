% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibull_mean.R
\name{weibull_mean}
\alias{weibull_mean}
\title{Mean of a Weibull Distribution given known Parameters}
\usage{
weibull_mean(shape, scale)
}
\arguments{
\item{shape}{The Weibull shape parameter.}

\item{scale}{The Weibull scale parameter.}
}
\value{
The output will be a numeric value indicating the Weibull mean.
}
\description{
\code{weibull_mean} the mean for a Weibull distribution based on
  the shape and scale parameters.
}
\examples{
# What is the mean of a Weibull distribution with shape 2 and scale 2000?
weibull_mean(2, 2000)
# What is the mean of a Weibull distribution with shape 1 and scale 2000?
weibull_mean(1, 2000)

}
\references{
/url{https://en.wikipedia.org/wiki/Weibull_distribution}
}
\seealso{
\code{\link{weibull_scale}}
}
