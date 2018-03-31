library(foreign) #Allows data to be imported from SPSS
        
Brit<-
   read.spss('c:/1data/regression iii/data/britain/brit9901.sav', use.value.labels=TRUE, to.data.frame=TRUE)

#   use.value.labels=TRUE imports value labels a categories
#   to.data.frame=TRUE imports to a data frame object
#   Of course, make sure the path is correct!

summary(Brit)

#gives a summary of the data frame         

attach(Brit)

#allows us to manipulate the objects (variables) in the dataframe

summary(age)

#   R is case sensitive!

summary(AGE)

library(car)

#The car package includes functions for recoding

variableA<-c(1,4,2,1,2,2,3,4,2,2,3,1,3,2,4,4,3,4,2,4,1)

#   Creating a variable called "variableA" using
#     the "concatenate" function

#   Recoding to collapse the categories of a quantitative variable

new.var1<-recode(variableA, "1:2=2; 3:4=1")
new.var1 [1:10]

#   Another way to do it

new.var2<-recode(variableA, "c(1,2)=1; else=3")
new.var2 [1:10]

#   Recoding a quantitative variable into a categorical variable

new.var3<-recode(variableA, "1:2='Low'; 3:4='High'")
new.var3 [1:10]

new.var4<-recode(variableA, "c(3,4)='Low'; else='High'")
new.var4 [1:10]

#   Re-specifying a quantitative variable as a factor (categorical var)

new.var5<-as.factor(variableA)
new.var5 [1:10]

#   Re-specifying a quantitative variable as an ordered factor

new.var6<-ordered(variableA, 
    levels=c('1', '3', '2', '4'))
new.var6
    

#   Examining distributions and relationships

library(car)

data(Duncan)
attach(Duncan)
summary(Duncan)

#   Univariate distributions

boxplot(prestige, main="Boxplot of prestige, Duncan data")
hist(prestige)
plot(density(prestige))

library(car)

qq.plot(prestige, labels=row.names(Duncan), col=1)

#   Bivariate relationships

boxplot(prestige ~ type, data = Duncan, col="yellow", 
  main='Boxplots of Income for different Occupation Types')

plot(income, prestige)
  abline(lm(prestige~income), lty=2)
  lines(lowess(income, prestige))

scatterplot(prestige, income, main="Scatterplot")

income2<-recode (income, "7:40='Low'; else='High'")
table(income2,type)

#   scatterplot is from the "car" library

#   Multivariate relationships

coplot(prestige~income|education, data=Duncan, panel=panel.smooth)

#   Component-plus-residual plots

Duncan.model1<-lm(prestige~income+education+type)
cr.plots(Duncan.model1)

#   Fitting a linear model

Duncan.model2<-lm(prestige~income*type+education)
summary(Duncan.model2)
#Plotting the interaction

library(lattice)

### 
#   Errors in the following lattice plot. Do not use!
###

trellis.device()
lset(col.whitebg())
plot(effects(Duncan.model2), multiline=TRUE)

detach(Duncan)

# Fitting a logit or probit model

data(Womenlf) # Data are from the car library
attach(Womenlf)
summary(Womenlf)

working <- recode(partic, " 'not.work' = 'no'; else = 'yes' ")
working[1:10]

logit.mod <- glm(working ~ hincome + children +region, 
                family=binomial)#Fitting a logit model
probit.mod <- glm(working ~ hincome + children +region, 
                family=binomial (link="probit"))#Fitting a probit model-"probit" link 
summary(probit.mod)
summary(logit.mod)
