% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repairable_systems_analysis.R
\name{ttt}
\alias{ttt}
\title{Total Time on Test for Repairable Systems}
\usage{
ttt(t, T, fail.trunc = FALSE)
}
\arguments{
\item{t}{A list of failure time vectors. Each vector should indicate
a different system, i.e. if you have multiple systems each
systems' failure times should be in it's own vector.}

\item{T}{A list of Total Time on Test (TTT) (i.e. test duration) vectors.
The vectors in the list should be of length 1, and each vector should
indicate a different system, i.e. if you have multiple systems each
systems' TTT should be in it's own vector.}

\item{fail.trunc}{Logical indicating if the test was failure terminated.}
}
\value{
The output will be a data.frame with the sorted supplied time
  values (\code{t}), the total time on test (\code{ttt}), and the
  scaled total time on test (\code{scaled_ttt}).
  A plot of \code{scaled_ttt} vs \code{ttt} would be a TTT plot.
}
\description{
\code{ttt} calculates the scaled total time on test (TTT) as described in
\href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.974&rep=rep1&type=pdf}{Tests for trend in more than one repairable system}
and \href{www.dtic.mil/dtic/tr/fulltext/u2/a043573.pdf}{Analysis of Time Between Failures for Repairable Components} \cr \cr
As indicated in \href{www.dtic.mil/dtic/tr/fulltext/u2/a043573.pdf}{Analysis of Time Between Failures for Repairable Components}
this can be used to create a TTT plot. \cr \cr
A TTT plot that follows the line \eqn{y = x}
indicates a Homogeneous Poisson Process (HPP) (constant failure rate),
while a concave TTT plot indicates a Nonhomogeneous Poisson Process (NHPP).
Concave up indicates decreasing failure rate,
while concave down indicates increasing failure rate. See:
\href{http://www.pinzhi.org/Minitab/Reliability_and_Survival_Analysis/RS_Growth_Curves/Total_time_on_test_plot.htm}{Minitab TTT Plot}
}
\examples{
data(amsaa)

# Three systems all time truncated at 200 hours
ttt_df <- ttt(
  t = split(amsaa$Time, amsaa$System),
  T = list(200,200,200),
  fail.trunc = FALSE)

theme_set(theme_bw())
ggplot(ttt_df, aes(x = ttt, y = scaled_ttt)) +
  geom_line(colour = "red") + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "Total Time on Test",
    y = "Scaled Total Time on Test")

}
\seealso{
\code{\link{power_law_process}}, \code{\link{power_law_mcf}},
  \code{\link{mcf}}, \code{\link{trend_test}}, \code{\link{common_beta}}
}
