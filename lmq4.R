library(stats4)
library(dplyr)

set.seed(120) #reproducability


lmq<-function(x,y){
  beta0=1 # init starting point
  beta1=1 # init starting point
  sigma=1 # init starting point
  q=1 # init starting point
 
  method = "L-BFGS-B" # or BFGS
  
  par(mfrow = c(1,1))	
  plot(x,y)
  
  logLikelihood <- function(beta0,beta1,sigma,q){
    beta= t(c(beta0,beta1)) # transpose and convert to matrix
    resid = y - beta0 - beta1*x
    R = dnorm(resid, mean = 0, sd=sqrt(x^(q*2)*sigma^2), log=TRUE)
    -sum(R)
  }
  
  mle(logLikelihood, list(beta0=beta0, beta1=beta1, sigma=sigma, q=q), method =method)
}


# test runs
x = rep(seq(1,3,by=0.5),2000)
y = 2 + 2*x + rnorm(length(x),0,2*x^4)
lmq(x,y)


# test runs
x = rep(seq(1,3,by=0.5),2000)
y = 6 + 4*x + rnorm(length(x),0,5*x^2)
lmq(x,y)



