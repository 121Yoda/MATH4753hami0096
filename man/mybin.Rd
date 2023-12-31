% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mybin.R
\name{mybin}
\alias{mybin}
\title{Binomial Simulation}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
\arguments{
\item{iter}{Number of samples to simulate}

\item{n}{Sample size}

\item{p}{Probability of success between 0 and 1}
}
\value{
A barplot of the simulation and a table of relative frequencies
}
\description{
Binomial Simulation
}
\examples{
mybin(iter=200,n=10, p=0.7)
}
