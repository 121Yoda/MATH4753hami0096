#' @title Flight Overbooking Seats
#'
#' @param N  Number of seats on a flight
#' @param gamma Probability of overbooking the flight ("pain" factor)
#' @param p Probability of a ticket holder showing
#'
#' @importFrom stats pbinom
#'
#' @return 2 plots and a list of values
#' @export
#'
#' @examples
#' ntickets(N=400,gamma=0.02,p=0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95){ #function definition with default values
  #calculate values first, then create plots

  #discrete
  n <- seq(N,floor(1.1*N),by=1) #a list of n values to "test"
  fd <- 1-gamma-pbinom(N,n,p) #the discrete objective function
  nd = n[which.min(abs(fd))] #which.min finds the index of the minimum of fd, using n[] finds the n value at this index

  #continuous
  fc <- function(x){ #the continuous objective function
    1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))
  } #defining a function this way means a list of continuous values for n need not be defined
  nc = stats::uniroot(fc,interval=c(n[1],n[length(n)]))$root #uniroot finds the n value at the zero of the given function

  #layout(1:2) ## if layout is used in Rmd, the plots are too small.

  #discrete plot
  plot(x=n,y=fd,type="b",xlab="n",ylab="Objective",bg="Blue",pch=21,
       main=paste0("Objective VS n: optimal tickets sold = ",nd,
                   "\nN = ",N,", gamma = ",gamma,", discrete"))
  abline(h=0,v=nd,col="Red")

  #continuous plot
  curve(fc,xlim = c(n[1],n[length(n)]),type="l",xlab="n",ylab="Objective",add=FALSE,
        main=paste0("Objective VS n: optimal tickets sold = ",round(nc,4),
                    "\nN = ",N,", gamma = ",gamma,", continuous"))
  abline(h=0,v=nc,col="Blue")

  list(nd=nd,nc=nc,N=N,p=p,gamma=gamma) #listing the important outputs
}
