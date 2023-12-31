% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymlnorm.R
\name{mymlnorm}
\alias{mymlnorm}
\title{Maximum Likelihood Estimate on a Normal Distribution}
\usage{
mymlnorm(x, mu, sig, ...)
}
\arguments{
\item{x}{A vector of sample data}

\item{mu}{A sequence of values to maximize for mu}

\item{sig}{A sequence of values to maximize for sigma}

\item{...}{Additional text to place on the graph}
}
\value{
A graph containing the MLEs of mu and sigma, and a list of key values
}
\description{
Maximum Likelihood Estimate on a Normal Distribution
}
\examples{
mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(10,15,length=1000),sig=seq(0.1,4,length=1000))
}
