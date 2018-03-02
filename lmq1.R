#ESTIMATING THE HETEROSKEDASTIC/AUTOCORRELATED LINEAR REGRESSION USING MLE



#The purpose of this session is to show you how to estimate and test the heteroskedastic and/or autocorrelated normal general linear models using MLE.



#HETEROSKEDASTICITY: Heteroskedasticity can be treated directly in the context of the normal MLE simply by specifying an equation to reflect the form of the heteroskedasticity in place of the variance term in the log likelihood function. This equation can take many different forms to correspond with the type of heteroskedasticity. There could simply be a weight that reflects the variance of each disturbance (White's approach), or the heteroskedasticity could be some linear combination of independent variables that may (or may not) be in the equation for the conditional mean. Other forms include dependent variable heteroskedasticity, where the weighting term is some function of the dependent variable (logs, absolute values, etc.), ARCH, and GARCH. All of these are easily treated in the context of the normal MLE. The example below shows one form, but this program could easily be modified to include all of the other forms.



# This file demonstrates how to alter the normal general linear model to encompass problems

# of heteroskedasticity and autocorrelation.



# Read in the data from Greene, 2003, chapter 11.

Credit <-
  read.table("C:/users/Wood/Documents/My Teaching/Maximum Likelihood/Data/credit.txt",
             header = TRUE)

attach(Credit)

summary(Credit)



# Create a new variable which is the square of income and add to the dataset

Credit$INCOME2 <- INCOME ^ 2

detach(Credit)

attach(Credit)



# Now delete the observations that we do not need, given the discussion in Greene.

Credit <- subset(Credit, subset = AVGEXP > 0)

detach(Credit)

attach(Credit)

summary(Credit)



# Now, estimate a least squares regression in which we suspect that heteroskedasticity is a function of income squared (INCOME2).

Credit.ols <-
  lm(AVGEXP ~ AGE + INCOME + INCOME2 + OWNRENT, data = Credit)

summary(Credit.ols)



# Let's examine the residuals plotted against the predicted values.



plot.default(predict(Credit.ols), resid(Credit.ols),  
             main = "Plot of Residuals Against Predicted Values")



# Let's examine the residuals plotted against income.



plot.default(INCOME, resid(Credit.ols), main = "Plot of Residuals Against Income")



# Now let's do some common tests for heteroskedasticity

library(lmtest)

# GOLDFELD-QUANDT TEST

gqtest(
  AVGEXP ~ AGE + INCOME + INCOME2 + OWNRENT,
  point = 0.5,
  order.by = ~ INCOME,
  data = (Credit)
)



# BREUSCH-PAGAN TEST

bptest(
  AVGEXP ~ AGE + INCOME + INCOME2 + OWNRENT,
  varformula = NULL,
  studentize = FALSE,
  data = (Credit)
)



# Bartlett Test

bartlett.test(Credit, data(Credit), subset(Credit, INCOME > 3), na.action)



# Now let's write a procedure to get robust standard errors (White correction)

# from White's Covariance Matrix

library(car)

robust.se <- function(model) {
  s <- summary(model)
  
  wse <- sqrt(diag(hccm(model)))
  
  t <- model$coefficients / wse
  
  df <- df.residual(model)
  
  p <- 2 * (1 - pt(abs(t), df))
  
  results <- cbind(model$coefficients, wse, t, p)
  
  dimnames(results) <- dimnames(s$coefficients)
  
  results
  
}

robust.se(Credit.ols)



# Now let's use weighted least squares

wls.model <-
  lm(AVGEXP ~ AGE + INCOME + INCOME2 + OWNRENT, weights = 1 / INCOME)

summary(wls.model)



# Now, let's develop our own ML procedure for dealing with heteroskedasticity

# using the optim command. We use the same procedure as last time with modifications.



# First, put the data into matrices for the MLE procedure

x <- cbind(1, AGE, INCOME, INCOME2, OWNRENT)

y <- as.matrix(AVGEXP)

z <- cbind(1, INCOME)

ones <- x[, 1]



# Calculate K (columns in x), k (columns in z), and n for later use

K <- ncol(x)

K

k <- ncol(z)

k

K1 <- K + 1

Kk <- K + k

K1

Kk

n <- nrow(x)

n



# Define the function to be optimized

llik.hregress <- function(par, X, Z, Y) {
  X <- as.matrix(x)
  
  Z <- as.matrix(z)
  
  Y <- as.vector(y)
  
  xbeta <- X %*% par[1:K]
  
  zgamma <- Z %*% par[K1:Kk]
  
  sum(-(1 / 2) * log(2 * pi) - (1 / 2) * log(exp(zgamma)) - (1 / (2 * exp(zgamma))) *
        (Y - xbeta) ^ 2)
  
}

llik.hregress



# Now let's use the above function to estimate the model.

hregress.model <-
  optim(
    c(0, 0, 0, 0, 0, 0, 0),
    llik.hregress,
    method = "BFGS",
    control = list(
      trace = 1,
      maxit = 1000,
      fnscale = -1
    ),
    
    hessian = TRUE
  )

hregress.model



# Note: different algorithms will make a difference to the non-significant estimates.

# Starting values set at 0 also matters; possible starting values from LIMDEP 32,-2.5,42,12,70,7,1



# Calculate the variance matrix from the Hessian matrix.

v <- -solve(hregress.model$hessian)

v



# Calculate the standard errors from the variance matrix.

se <- sqrt(diag(v))

se



# Calculate the z statistics from the coefficients and standard errors

b <- hregress.model$par

b

zstat <- b / se

zstat



# Calculate p values from the z statistics

pz <- 2 * pnorm(-abs(zstat))



# Put the results together in a table.

table <- cbind(b, se, zstat, pz)

table



# We can test hypotheses about the preceding model using an F or Wald test.

# The F and Wald tests are just scalar versions of one another. Here is a Wald test

# that the coefficient on income in the variance equation is zero.



r <- rbind(c(0, 0, 0, 0, 0, 0, 1))

q <- c(0)

j <- nrow(r)



Wald <- t(r %*% b - q) %*% solve (r %*% v %*% t(r)) %*% (r %*% b - q)

Wald

Waldpvalue <- 1 - pchisq(Wald, df = j)

Waldpvalue



# Here are Wald  and F tests that the coefficients on income and income2 are

# jointly zero in the main equation.



r <- rbind(c(0, 0, 1, 0, 0, 0, 0), c(0, 0, 0, 1, 0, 0, 0))

q <- c(0)

j <- nrow(r)



Wald <- t(r %*% b - q) %*% solve (r %*% v %*% t(r)) %*% (r %*% b - q)

Wald



F <- Wald / j

F

Fpvalue <- 1 - pf(F, df1 = j, df2 = n - K)

Fpvalue



# We can also do a likelihood ratio test by reestimating the model with imposed restrictions.

# For example, here is the implementation of the likelihood ratio test for the preceding hypothesis.

# First, save the log likelihood from the previous model for later use below.



LU <- hregress.model$value

LU



# Now reestimate with the restrictions.



# Remove income and income2 from the data matrix

x <- cbind(1, AGE, OWNRENT)

y <- as.matrix(AVGEXP)

z <- cbind(1, INCOME)

ones <- x[, 1]



# Calculate K (columns in x), k (columns in z), and n for later use

K <- ncol(x)

K

k <- ncol(z)

k

K1 <- K + 1

Kk <- K + k

K1

Kk

n <- nrow(x)

n



# Define the function to be optimized

llik.hregressR <- function(par, X, Z, Y) {
  X <- as.matrix(x)
  
  Z <- as.matrix(z)
  
  Y <- as.vector(y)
  
  xbeta <- X %*% par[1:K]
  
  zgamma <- Z %*% par[K1:Kk]
  
  sum(-(1 / 2) * log(2 * pi) - (1 / 2) * log(exp(zgamma)) - (1 / (2 * exp(zgamma))) *
        (Y - xbeta) ^ 2)
  
}

llik.hregressR



# Now let's use the above function to estimate the model.

hregressR.model <-
  optim(
    c(0, 0, 0, 0, 0),
    llik.hregressR,
    method = "BFGS",
    control = list(
      trace = 1,
      maxit = 1000,
      fnscale = -1
    ),
    
    hessian = TRUE
  )

hregressR.model



# Save the restricted log likelihood.

LR <- hregressR.model$value

LR



# Calculate the likelihood ratio test



LLRatio <- -2 * (LR - LU)

LLRatio

LLRatioPvalue <- 1 - pchisq(LLRatio, df = 1)

LLRatioPvalue



# That's all for heteroskedasticity. Let's detach the data file and remove all objects.

detach(Credit)

rm(list = ls(all = TRUE))

ls()





# This next part of the file demonstrates various methods for dealing with autocorrelation

# within R.



# First read the data. We return to the Ostrom data.

Ostrom <-
  read.table("C:/users/Wood/Documents/My Teaching/Maximum Likelihood/Data/ostrom.dat",
             header = TRUE)

attach(Ostrom)

Ostrom

summary(Ostrom)



# First, make time series objects for analysis

US <- ts(US)

USSR <- ts(USSR)



# Now let's fit a regression the easy way and plot the residuals

mod.ols <- lm(US ~ USSR)

summary(mod.ols)



plot(year, resid(mod.ols), main = "Plot of Residuals Against Year", type =
       'o')

abline(h = 0, lty = 2)



# Let's plot the autocorrelation and partial autocorrelation functions



par(mfrow = c(2, 1))

acf(residuals(mod.ols))

acf(residuals(mod.ols), type = 'partial')



# Let's get the Durbin-Watson statistic.

durbinWatsonTest(mod.ols)



# Let's get the Breusch-Godfrey statistic with 2 lags.

library(lmtest)

bgtest(US ~ USSR, order = 2)



# There is clear evidence of autocorrelation. Obtaining GLS estimates

library(nlme)

mod.gls <-
  gls(US ~ USSR,
      correlation = corAR1(form = ~ 1 | USSR),
      method = 'ML')

summary(mod.gls)



# Now let's compute robust (autocorrelation and heteroskedasticity consistent)

# standard errors and t statistics using the Newey-West procedure.

# earlier versions required library(sandwich)

# First, get the robust covariance matrix and then compute the standard errors.

library(sandwich)

b <- mod.ols$coefficients

b

se <- sqrt(diag(vcovHAC(mod.ols)))

se

tstat <- b / se

tstat

ptvalue  <- 2 * (1 - pt(tstat, df = df.residual(mod.ols)))

ptvalue



# Put results into a table.

table <- cbind(b, se, tstat, ptvalue)

table



# Now let's develop a maximum likelihood procedure using optim. Note that unlike the "canned" GLS

# procedure above, we will use matrices in optimizing the log likelihood function. The log likelihood

# we will use comes from Greene 2003, p. 273



# Put the data into matrices for the MLE procedure

x <- cbind(1, USSR)

y <- as.matrix(US)



# Calculate K (columns in x) and n for later use

K <- ncol(x)

K

K1 <- K + 1

K2 <- K + 2

n <- nrow(x)

n



# Define the function to be optimized

llik.arregress <- function(par, X, Y) {
  X <- as.matrix(x)
  
  Y <- as.vector(y)
  
  xbeta <- X %*% par[1:K]
  
  rho <- par[K1:K1]
  
  sig <- par[K2:K2]
  
  P <- matrix(0, nrow = n, ncol = n)
  
  P[row(P) == col(P)] <- 1
  
  P[row(P) == col(P) + 1] <- (-rho)
  
  P[1, 1] = sqrt(1 - rho ^ 2)
  
  sum(
    -(1 / (2 * n * sig ^ 2)) * t(Y - xbeta) %*% (t(P) %*% P) %*% (Y - xbeta) + (1 /
                                                                                  (2 * n)) * log(1 - rho ^ 2)
    
    - (1 / 2) * (log(2 * pi) + log(sig ^ 2))
  )
  
}

llik.arregress



ar.model <-
  optim(
    c(15, 0.9, .8, 13),
    llik.arregress,
    method = "BFGS",
    control = list(
      trace = 1,
      maxit = 1000,
      fnscale = -1
    ),
    
    hessian = TRUE
  )

ar.model



# Calculate the variance matrix from the Hessian matrix.

v <- -solve(ar.model$hessian)

v



# Calculate the standard errors from the variance matrix.

se <- sqrt(diag(v))

se



# Calculate the z statistics from the coefficients and standard errors

b <- ar.model$par

b

zstat <- b / se

zstat



zpvalues <- 2 * (1 - pnorm(abs(zstat)))

zpvalues



# Put the results together in a table.

table <- cbind(b, se, zstat, zpvalues)

table



# That's all for autocorrelation.