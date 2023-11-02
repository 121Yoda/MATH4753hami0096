#' @title Central Limit Theorem on a Uniform
#'
#' @param n Sample size
#' @param iter Number of samples to take
#' @param a Minimum value of the uniform distribution
#' @param b Maximum value of the uniform distribution
#'
#' @importFrom graphics lines curve
#' @importFrom stats runif density dnorm dunif
#'
#' @return A sophisticated histogram of the distribution of the sample means
#' @export
#'
#' @examples
#' mycltu(n=20,iter=100000,a=0,b=5)
mycltu=function(n,iter,a=0,b=1){
  x=NULL
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
