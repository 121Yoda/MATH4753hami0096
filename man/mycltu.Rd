% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltu.R
\name{mycltu}
\alias{mycltu}
\title{Central Limit Theorem on a Uniform}
\usage{
mycltu(n, iter, a = 0, b = 1)
}
\arguments{
\item{n}{Sample size}

\item{iter}{Number of samples to take}

\item{a}{Minimum value of the uniform distribution}

\item{b}{Maximum value of the uniform distribution}
}
\value{
A sophisticated histogram of the distribution of the sample means
}
\description{
Central Limit Theorem on a Uniform
}
\examples{
mycltu(n=20,iter=100000,a=0,b=5)
}
