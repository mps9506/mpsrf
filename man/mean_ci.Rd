% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_geomean.R
\name{mean_ci}
\alias{mean_ci}
\title{Calculate Mean and Confidence Intervals}
\usage{
mean_ci(x, ci, R = 1000)
}
\arguments{
\item{x}{numeric vector}

\item{ci}{desired confidence interval}

\item{R}{number of resamples}
}
\value{
named list with mean and confidence interval
}
\description{
Calculates the bootstrap percentile confidence intervals around the mean
}
\keyword{internal}
