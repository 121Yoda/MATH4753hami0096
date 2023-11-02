#' @title Binomial Simulation
#'
#' @param iter Number of samples to simulate
#' @param n Sample size
#' @param p Probability of success between 0 and 1
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @return A barplot of the simulation and a table of relative frequencies
#' @export
#'
#' @examples
#' mybin(iter=200,n=10, p=0.7)
mybin=function(iter=100,n=10, p=0.5){
  # matrix of NA to hold the samples
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  # vector to hold the number of successes per trial
  succ=c()
  for( i in 1:iter){
    # each column is a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    # sum the samples
    succ[i]=sum(sam.mat[,i])
  }
  # table of successes
  succ.tab=table(factor(succ,levels=0:n))
  # barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
