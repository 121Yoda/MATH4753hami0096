#' @title 95\% Confidence Interval for Mean
#'
#' @param x A sample vector from which to estimate a population mean
#'
#' @importFrom stats qt
#'
#' @return A 95\% confidence interval for the mean, and an invisible list of key values
#' @export
#'
#' @examples
#' myci(rnorm(2,mean=5,sd=4))
myci <- function(x){
  n=length(x)
  alpha=0.05
  t=qt(1-alpha/2,n-1)
  mp=c(-1,1)
  mean=mean(x)
  sd=sd(x)
  ci=mean+mp*t*sd/sqrt(n)
  print(ci)
  invisible(list(x=x,n=n,alpha=alpha,mean=mean,sd=sd,ci=ci))
}
