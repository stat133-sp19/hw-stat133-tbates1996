% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{plot.bincum}
\alias{plot.bincum}
\title{Plot cumulative binomial distribution}
\usage{
\method{plot}{bincum}(cum)
}
\arguments{
\item{cum}{the cumulative binomial variable bincum}
}
\description{
Plots a line of the cumulative binomial distribution
}
\examples{
dis1 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis1)
}
