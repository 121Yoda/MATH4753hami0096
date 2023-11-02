#' @title Normal Curve and Probability
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Lower tail probability cutoff
#'
#' @importFrom graphics polygon
#' @importFrom stats pnorm
#'
#' @return A normal curve shaded with a lower tail probability, and a probability value
#' @export
#'
#' @examples
#' myncurve(10,4,9)
myncurve = function(mu, sigma, a){
  x=NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length = 1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(x = c(mu-3*sigma,xcurve,a),y = c(0,ycurve,0),col = "Red")

  prob = pnorm(a,mean=mu,sd=sigma)
  prob2 <- round(prob,4)
  prob2
}
