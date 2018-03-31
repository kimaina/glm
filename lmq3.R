library(stats4)

set.seed(103)

x <- runif(10000,min=0,max=3)
y <- 2 + 0.5*x + rnorm(1000,0,3)

par(mfrow = c(1,1))	
plot(x,y)
LL <- function(beta1, beta0, mu, sigma){
  resid <- y-x*beta1 - beta0
  R = dnorm(resid,mu,sigma)
  -sum(log(R))
  
}

mle(LL, start= list(beta1= as.numeric(lm(y~x)[[1]][2]), beta0 = 
                      as.numeric(lm(y~x)[[1]][1]), mu = 0.01, sigma = sd(y)), 
    method = "L-BFGS-B", lower = c(-Inf, -Inf, -Inf, 0), upper = c(Inf, Inf, Inf, Inf))