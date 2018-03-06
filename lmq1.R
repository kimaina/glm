library(stats4)
library(dplyr)

set.seed(100)
x = rep(seq(1,3,by=0.5),2000)
y = 1 + 2*x + rnorm(length(x),0,1*x^1)

par(mfrow = c(1,1))	
plot(x,y)

LL <- function(beta0,beta1,sigma,q){
  beta= t(c(beta0,beta1))
  resid = y - beta0 - beta1*x
  R = dnorm(resid, mean = 0, sd=sqrt(x^(q*2)*sigma^2), log=TRUE)
  -sum(R)
}

mle(LL, list(beta0=2, beta1=1, sigma=1, q=1), method = "L-BFGS-B")