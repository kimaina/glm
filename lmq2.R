#ESTIMATING A LINEAR REGRESSION USING MLE



#The purpose of this session is to introduce you to the MLE of the normal general linear model. This approach to linear regression” forms the statistical basis for hypothesis testing found in most econometrics textbooks. Thus, the normal general linear model is important from the standpoint of justifying least squares. More important, this model serves as a tool for understanding maximum likelihood estimation of many time series models, models with heteroskedastic disturbances, and models with non-normal disturbances.



# This file calculates a regression using maximum likelihood.



# Read in the Ostrom data



Ostrom <- read.table("C:/users/Wood/Documents/My Teaching/Maximum Likelihood/Data/ostrom.dat", header=TRUE)

attach(Ostrom)

Ostrom

summary(Ostrom)



# Generate time series plots of the data.

par(mfrow=c(2,1))

plot(year,USSR)

plot(year,US)



# Plot US versus USSR

plot(USSR,US)



# First, calculate a regression the easy way, plot the

# residuals, and do an F test for whether the coefficient on USSR=0

OLS.model <- lm(US ~ USSR , data=Ostrom)

summary(OLS.model)

plot(residuals(OLS.model))



library(car)

# Set up the F test

r <- c(0,1)

q <- c(0)

linearHypothesis(OLS.model, r, q)



# We can also get various diagnostics from the preceding model as follows.

# These are only a few. We explore diagnostics for heteroskedasticity and autocorrelation in the next lesson.

anova(OLS.model)

predict(OLS.model)

logLik(OLS.model)

AIC(OLS.model)

influence.measures(OLS.model)

library(lmtest)

reset(US ~ USSR, power=3, type='fitted')



# Now let's fit the Normal Maximum Likelihood model.



# First, put the data into matrices for the MLE procedure

x <- cbind(1,as.matrix(USSR))

y <- as.matrix(US)

ones <- x[,1]



# Calculate K and n for later use

K <- ncol(x)

K

K1 <- K + 1

n <- nrow(x)

n



# Define the function to be optimized



llik.regress <- function(par,X,Y) {
  
  Y <- as.vector(y)
  
  X <- as.matrix(x)
  
  xbeta <- X%*%par[1:K]
  
  Sig <- par[K1:K1]
  
  sum(-(1/2)*log(2*pi)-(1/2)*log(Sig^2)-(1/(2*Sig^2))*(y-xbeta)^2)
  
}

llik.regress



# Now let's use the above function to estimate the model.



model <- optim(c(15,0.9,13),llik.regress, method = "BFGS", control = list(trace=1,maxit=100,fnscale = -1),
               
               hessian = TRUE)

model



# Notice that the coefficients from this model are the same as those produced through OLS estimation

#Now let's get the variances, standard errors, Z statistics, and log likelihood from our model.



# Calculate the variance matrix from the Hessian matrix. 



v <- -solve(model$hessian)

v



# Calculate the standard errors from the variance matrix.



se <- sqrt( diag(v))

se



# Calculate the z statistics from the coefficients and standard errors



b <- model$par

b

zstat <-b/se

zstat



# Calculate p-values for the z statistics

pzstat <- 2* (1 - pnorm(abs(zstat)))

pzstat



# Put the results together in a table.

table <- cbind(b,se,zstat,pzstat)

table





# Note that the estimate of the error variance is biased by n/n-K

# we can correct the preceding estimate of the standard error of #estimates as follows

Sig <- model$par[3]

Sig

Sigunb <- sqrt(n/(n-K)*Sig^2)

Sigunb



# The square of the z-statistic on USSR is a Wald test that USSR=0. We might want to

# test this hypothesis with other methods, including either a likelihood ratio or LaGrange

# multiplier. Therefore, we save the log-likelihood for later testing below. 

LU <- model$value

LU



# Now let's do a likelihood ratio test that the coefficient on USSR=0 using "LU" from above.

# First, restimate the model with the restriction that USSR=0.  Save the log likelihood from this model



llik.regressR <- function(par,X,Y) {
  
  Y <- as.vector(y)
  
  X <- as.matrix(ones)
  
  xbeta <- X%*%par[1:K-1]
  
  Sig <- par[K:K]
  
  sum(-(1/2)*log(2*pi)-(1/2)*log(Sig^2)-(1/(2*Sig^2))*(y-xbeta)^2)
  
}

llik.regressR



# Now let's use the above function to estimate the model.



modelR <- optim(c(140,74),llik.regressR, method = "CG", control = list(trace=1,maxit=100,fnscale = -1),
                
                hessian = TRUE)

modelR

LR <- modelR$value

LR



# Now calculate the likelihood ratio test using the saved values of the log likelihood function.*/



llratio <- -2*(LR-LU)

llratio

llratioPvalue <- 1-pchisq(llratio,K-1)

llratioPvalue





# In some instances it will be useful to perform matrix calculations on MLE outputs. For example,

# in computing quadratic forms. As an example of using matrices, let's illustrate the preceding

# regression using some of the matrix capabilities of R



# We defined the appropriate matrices for this above.



# Compute coefficient estimates using matrices.



bols <- solve(t(x) %*% x) %*% t(x) %*% y

bols



# Compute and list predicted values and residuals



yhat <- x %*% bols

yhat

errors <- y - yhat   

errors



# Compute and display the maximum likelihood estimate of the error variance



SSE <- t(errors) %*% errors

SSE

S2 <- (1/n) * SSE

S2



# Compute the maximum likelihood covariance matrix of coefficients. The diagonal of this

# matrix contains the standard errors.



covb <-  as.numeric(S2) * (solve(t(x) %*% x))

covb



# Extract the standard errors



se <- sqrt(diag(covb))

se



# Calculate t statistics



tstat <- bols/se

tstat