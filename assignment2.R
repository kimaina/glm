#' ---	
#' title: "Homework 2"	
#' author: "Allan Kimaina"	
#' date: "February 22, 2018"	
#' header-includes:	
#' - \usepackage{pdflscape}	
#' - \newcommand{\blandscape}{\begin{landscape}}	
#' - \newcommand{\elandscape}{\end{landscape}}	
#' output:	
#'   pdf_document: default	
#'   html_document: default	
#' ---	
#' 	
#' 	
#knitr::opts_chunk$set(echo = F)	
	
# load package	
library(dplyr)	
library(car)	
library(sjPlot)	
library(sjmisc)	
library(sjlabelled)	
library(ggpubr)	
library(ggpmisc)	
library("gridExtra")	
library(stargazer)	
library(e1071)	
library(jtools)	
library(effects)	
library(multcompView)	
library(ggplot2)	
library(ggrepel)	
library(MASS)	
library(broom)	
library(ggcorrplot)	
library(leaps)	
library(relaimpo)	
library(olsrr)	
	
	
# load all data	
wine = read.csv("data/Wine.csv")	
bush = read.csv("data/Bush.csv")	
bloodBrain = read.csv("data/BloodBrain.csv")	
	
# clean bush	
colnames(bush)[1] <- "county" # name the 1st column using standard	
col2cvt <- 2:ncol(bush) # get all numeric columns	
bush[,col2cvt] <- lapply(bush[,col2cvt],function(x){as.numeric(gsub(",", "", x))}) # convert to numeric	
	
# clean wine	
colnames(wine)[1] <- "COUNTRY"	
	
# clean wine	
colnames(bloodBrain)[1] <- "BRAIN"	
	
#' 	
#' \onecolumn	
#' 	
#' # Question 1	
#' 	
#' ## a. Using the wine data from the previous homework, create two new variables by logarithmically transforming both wine and     mortality	
#' 	
#' 	
	
wine$logMORTALITY <- log(wine$MORTALITY)	
wine$logWINE <- log(wine$WINE)	
	
# print	
stargazer(wine,	
          header=F,	
          type = "latex",	
          no.space = T,	
          summary = F,	
          single.row = T	
          )	
	
#' 	
#' 	
#' ## b. Plot mortality vs. wine; log mortality vs. wine, mortality vs. log wine and log mortality vs. log wine. Which one looks more   linear?	
#' 	
#' 	
	
wine.model.plot = ggplot(wine, aes(y=MORTALITY, x=WINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    labs(x='WINE', y='MORTALITY',	
         title = "Mortality vs Wine")+	
   # scale_x_log10() +	
   # scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
wine.model.log.x.plot=ggplot(wine, aes(y=MORTALITY, x=logWINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    labs(x='log(WINE)', y='MORTALITY',	
         title = "Mortality vs log(Wine)")+	
  	
    #scale_x_log10() +	
    #scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
wine.model.log.y.plot=ggplot(wine, aes(y=logMORTALITY, x=WINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    labs(x='WINE', y='log(MORTALITY)',	
         title = "log(Mortality) vs Wine")+	
   	
   # scale_x_log10() +	
   # scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
	
wine.model.log.xy.plot =ggplot(wine, aes(y=logMORTALITY, x=logWINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    labs(x='log(Wine)', y='log(Mortality)',	
         title = "log(Mortality) vs log(Wine)")+	
   	
   #scale_x_log10() +	
    #scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
  grid.arrange(wine.model.plot,	
               wine.model.log.x.plot,	
               wine.model.log.y.plot,	
               wine.model.log.xy.plot,	
               ncol =2.,	
               nrow = 2.)	
#' 	
#' 	
#' - From the above scatter plot, the logarithmic transformation of both dependent variable (log of Mortality) and independent variable (log of Wine) results in the most linear relationship compared to the other plots.	
#' - Transforming both predictor and response variable we get the best linear relationship	
#' 	
#' ## c. Fit four linear regression models corresponding to the four plots and report the regression equation and R-squared. Which model do you think is best?	
#' 	
#' 	
#' * **Mortality vs Wine:** \(HeartAttackMortality =  7.69- 0.0761*WineConsumption + \epsilon\)	
#' * **Mortality vs log(Wine):**  \(HeartAttackMortality =  10.280 - 1.771 *log(WineConsumption) + \epsilon\)	
#' * **log(Mortality) vs Wine:**  \(log(HeartAttackMortality) =  2.0453 - 0.0159 *WineConsumption + \epsilon\)	
#' * **log(Mortality) vs log(Wine):** \(log(HeartAttackMortality) =  2.5556 - 0.3556*log(WineConsumption) + \epsilon\)	
#' 	
#' 	
#' 	
	
wine.model.plot.1=ggplot(wine, aes(y=MORTALITY, x=WINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='WINE', y='MORTALITY',	
         title = "Mortality vs Wine")+	
    stat_poly_eq(aes(label = paste( ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
                 label.x.npc = "right", label.y.npc = 0.87, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
   stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")),	
                 label.x.npc = "left", label.y.npc = 0.001, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
    stat_fit_glance(method = "lm",	
                    method.args = list(formula = y~x),	
                    geom = "text", size = 3,	
                    label.y.npc = 0.85, label.x.npc = "right",	
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
   # scale_x_log10() +	
   # scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
	
wine.model.plot.2=ggplot(wine, aes(y=MORTALITY, x=logWINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='log(WINE)', y='MORTALITY',	
         title = "Mortality vs log(Wine)")+	
    stat_poly_eq(aes(label = paste( ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
                 label.x.npc = "right", label.y.npc = 0.87, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
   stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")),	
                 label.x.npc = "left", label.y.npc = 0.03, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
    stat_fit_glance(method = "lm",	
                    method.args = list(formula = y~x),	
                    geom = "text", size = 3,	
                    label.y.npc = 0.85, label.x.npc = "right",	
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
    #scale_x_log10() +	
    #scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
	
wine.model.plot.3=ggplot(wine, aes(y=logMORTALITY, x=WINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='WINE', y='log(MORTALITY)',	
         title = "log(Mortality) vs Wine")+	
    stat_poly_eq(aes(label = paste( ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
                 label.x.npc = "right", label.y.npc = 0.87, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
   stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")),	
                 label.x.npc = "left", label.y.npc = 0.03, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
    stat_fit_glance(method = "lm",	
                    method.args = list(formula = y~x),	
                    geom = "text", size = 3,	
                    label.y.npc = 0.85, label.x.npc = "right",	
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
   # scale_x_log10() +	
    #scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
	
wine.model.plot.4=ggplot(wine, aes(y=logMORTALITY, x=logWINE)) +	
    geom_point(alpha = .5) +	
    geom_point(color = "blue") +	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='log(Wine)', y='log(Mortality)',	
         title = "log(Mortality) vs log(Wine)")+	
    stat_poly_eq(aes(label = paste( ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
                 label.x.npc = "right", label.y.npc = 0.87, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")),	
                 label.x.npc = "left", label.y.npc = 0.15, geom = "text",	
                 formula =  y ~x, parse = TRUE, size = 3)+	
    stat_fit_glance(method = "lm",	
                    method.args = list(formula = y~x),	
                    geom = "text", size = 3,	
                    label.y.npc = 0.85, label.x.npc = "right",	
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
   #scale_x_log10() +	
    #scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
  grid.arrange(wine.model.plot.1,	
               wine.model.plot.2,	
               wine.model.plot.3,	
               wine.model.plot.4,	
               ncol =2.,	
               nrow = 2.)	
	
#' 	
#' 	
#' Graphically we notice that the regression line for the log-transformed model (ln(y) ~ ln(x)) covers most data-points as well as having a more precise and uniform CI band compared with the other 3 models. 	
#' 	
#' 	
wine.model <- lm(MORTALITY~WINE, data=wine )	
wine.model.log.x <- lm( MORTALITY~logWINE, data=wine )	
wine.model.log.y <- lm( logMORTALITY~WINE, data=wine)	
wine.model.log.xy <- lm( logMORTALITY~logWINE, data=wine )	
	
	
stargazer(wine.model,wine.model.log.x,wine.model.log.y,wine.model.log.xy,	
          header=F,	
          type = "latex",	
          summary = F,	
          no.space = T,	
          ci = F,	
         #keep = c("\\bprecip\\b"),	
          title = "Comparison of the 4 Models",	
          column.labels = c( "y~x", "y~ln(x)","ln(y)~x", "ln(y) vs ln(x)"),	
          notes = "t-values have been hidden!",	
            dep.var.caption  = "-" ,  # Bold	
          single.row = T	
	
          )	
	
#' 	
#' 	
#' Looking at y~x and y~ln(x) models, we have really low explanatory power (R-squared). In  y~x model, only 56% of the variability in heart attack mortality is explained by wine consumption. While in y~ln(x) model only  64% of the variability in the response is explained by the predictor.	
#' 	
#' Comparing the explanatory power between the  ln(y)~x model and ln(y)~ln(x) model, we get a higher explanatory power in ln(y)~ln(x) model. Over 73.6% of the variability in heart attack mortality is explained by wine consumption in ln(y)~ln(x) model while only 73.6% of the variability in heart attack mortality is explained by wine consumption in ln(y)~x model	
#' 	
#' For these reasons,  **the best fit model** is ln(y)~ln(x) model because it has the highest explanatory model compared to the other models. Furthermore, it has a p-value that is very significant (approximately 4.914e-06) 	
#' 	
#' ### Model Diagnostics 	
#' 	
#' 	
par( mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid	
# model 1	
plot(wine.model, main = "Mortality ~ Wine",  which=c(1))	
	
	
# model 2	
plot(wine.model.log.x, main = "Mortality  ~  ln(Wine)",  which=c(1))	
	
# model 3	
plot(wine.model.log.y, main = "ln(Mortality)  ~  Wine",  which=c(1))	
	
# model 4	
plot(wine.model.log.xy, main = "ln(Mortality)  ~  ln(Wine)",  which=c(1))	
	
#' 	
#' 	
#' Looking at the residual plots,  ln(Mortality)  ~  ln(Wine) model exhibits acceptable randomness and unpredictability. From the plot, the residuals bounce randomly around the 0 line and roughly forms a horizontal band around the 0 line without any noticeable pattern. This suggests that the variances of the error terms are approximately equal. 	
#' 	
#' The other models seem not to follow the rule of residual heteroskedasticity which is a crucial component of any regression model. Residuals in the other models deviate from linearity and equality of standard deviations assumptions. Hence this gives us more reason to select the model with logarithmic transformation of both predictor and response as the best fit model	
#' 	
#' ### Conclusion	
#' 	
#' In summary, the model with the natural log of heartAttackMortality as the response and the natural log of wineConsumption as the predictor exhibits a relationship with the highest explanatory power. This relationship appears to be linear and at the same time with error terms that appear independent and normally distributed with somewhat equal variances. Therefore we have sufficient evidence that this model [ln(y)~ln(x)] is the best.	
#' 	
#' 	
#' ## d. Interpret each regression model by describing the change in mortality for a given change in the predictor. Use an increase of 10 units of wine consumption for linear wine and a doubling of wine consumption for logarithmic wine	
#' 	
#' 	
#' ### Mortality vs Wine	
#' 	
#' - An increase of wine consumption by 10 Liters per person will decrease Ischemic heart attack mortality by 0.761 per 1000 person	
#' 	
#' ### Mortality vs log(Wine)	
#' 	
#' - Doubling wine consumption will decrease Ischemic heart attack mortality rate by 1.227 per 1000 person	
#' 	
#' ### log(Mortality) vs Wine	
#' 	
#' medianProportion = exp(-0.0159*10) =  0.8529964	
#' %change = (exp(-0.0159*10)-1)*100 = -14.7%	
#' 	
#' - An increase of wine consumption by 10 Liters per person will decrease Ischemic heart attack mortality by a median proportion of 0.8529964	
#' - An increase of wine consumption by 10 Liters per person will decrease Ischemic heart attack mortality by 14.7%	
#' 	
#' ### log(Mortality) vs log(Wine)	
#' 	
#' %change = 1-exp(-0.3556*log(2)))*100 = -21.84555	
#' 	
#' - Doubling wine consumption will decrease Ischemic heart attack mortality by 21.85%	
#' 	
#' \onecolumn	
#' 	
#' # Question 2: The Dramatic U.S. Presidential Election of 2000	
#' 	
#' The U.S. presidential election of November 7, 2000 was one of the closest in history. As returns were counted on election night it became clear that the outcome in the state of Florida would determine the next president. At one point in the evening, television networks projected that the state was carried by the Democratic nominee, Al Gore, but a retraction of the projection followed a few hours later. Then, early in the morning of November 8, the networks projected that the Republican nominee, George W. Bush, had carried Florida and won the presidency. Gore called Bush to concede. While on route to his concession speech, though, the Florida count changed rapidly in his favor. The networks once again reversed their projection, and Gore called Bush to retract his concession. When the roughly 6 million Florida votes had been counted	
#' 	
#' 	
#' 	
#' 	
#' ## a. The data in File Bush.xls contain the numbers of votes for Buchanan and Bush in all 67 counties in Florida. What evidence is there in the scatterplot of Display 8.25 that Buchanan received more votes than expected in Palm Beach County? Analyze the data without Palm Beach County results to obtain an equation for predicting Buchanan votes from Bush votes. Obtain a 95% prediction interval for the number of Buchanan votes in Palm Beach from this result-assuming the relationship is the same in this county as in the others. If it is assumed that Buchanan's actual count contains a number of votes intended for Gore, what can be said about the likely size of this number from the prediction interval? (Consider transformation.)	
#' 	
#' ![Caption for the picture.](scatter.PNG)	
#' 	
#' From the scatter plot  (Display 8.25) we can vividly see that Palm Beach County is one of the outlier because it has been separated away from the trend of other observation.  In fact, if we regress Buchanan on Bush using the full data set with this potential outlier, the studentized residual and cooks distance flags this datapoint because it influences the regression model to such an extent that the estimated regression equation is pulled towards this datapoint.	
#' 	
#' 	
bush <- bush %>%	
  mutate(total96=clinton96+dole96+perot96+buchanan96p)	
	
# transformation	
bush.log <- bush	
bush.log$reform.reg = bush.log$reform.reg+0.00000001	
for(i in 2:ncol(bush.log)){	
  bush.log[,i] <- log(bush.log[,i])	
}	
	
bush.withPalmBeach.model <- lm(buchanan2000~bush2000, data=bush.log)	
	
par( mfrow = c(1, 2)) 	
	
# identify D values > 4/(n-k-1) 	
cutoff <- 4/((nrow(bush)-length(bush.withPalmBeach.model$coefficients)-2)) 	
plot(bush.withPalmBeach.model, which=4, cook.levels=cutoff)	
# Influence Plot 	
influencePlot(bush.withPalmBeach.model,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )	
#' 	
#' 	
#' 	
#' 	
#' 	
	
	
# What evidence is there in the scatterplot of Display 8.25 that Buchanan received more votes than expected in Palm Beach County?	
# - Palm Beach County is an outlier	
	
bushPalmBeach <- bush.log %>%	
                      filter(county=='PALM BEACH')	
	
	
bush.withoutPalmBeach <- bush.log %>%	
                            filter(county!='PALM BEACH')	
	
	
bush.withoutPalmBeach.model <- lm(buchanan2000~bush2000, data=bush.withoutPalmBeach)	
	
	
stargazer(bush.withPalmBeach.model,	bush.withoutPalmBeach.model,	
          header=F,		
          type = "latex",		
          summary = F,		
          no.space = T,		
          ci = TRUE,		
         keep = c("\\bprecip\\b"),		
          title = "Models With and without outlier", 		
          column.labels = c( "Without Palm Beach", "With Palm Beach"),		
          notes = "Coefficients have been removed!",		
            dep.var.caption  = "-" ,  # Bold		
          single.row = T		
         		
          )		
	
#' 	
#' 	
#' Furthermore, if we remove this potential outlier from the dataset and regress Buchanan on Bush, our regression model changes substantially in the sense that residual standard error is inflated from 413 to 371. This inflation highly increases  the width of confidence and prediction intervals	
#' 	
#' 	
	
bush.withoutPalmBeach.plot = ggplot(bush.withoutPalmBeach,	
                                    aes(y=buchanan2000, x=bush2000	
                                        )) +	
    geom_point(alpha = .5) +	
     geom_point(color = "black") +	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='log(bush2000)', y='log(buchanan2000)',	
         title = "Linear Model Without Palm Beach")+	
    # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
    #              label.x.npc = "right", label.y.npc = 0.17, geom = "text",	
    #              formula =  y ~x, parse = TRUE, size = 3)+	
    # stat_fit_glance(method = "lm",	
    #                 method.args = list(formula = y~x),	
    #                 geom = "text", size = 3,	
    #                 label.y.npc = 0.15, label.x.npc = "right",	
    #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
   # scale_x_log10() +	
   # scale_y_log10()+	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
	
	
  bush.withPalmBeach.plot = ggplot(bush.log, aes(y=buchanan2000, x=bush2000, color=ifelse(((abs(buchanan2000)>8 )),"A", "B"))) +	
    geom_point(alpha = .5) +	
    geom_point() +	
    scale_color_manual(guide=FALSE, values=c("red", "black")) + #turn off the legend, define the colors	
    geom_text_repel(data = subset(bush.log, buchanan2000 > 8), aes(label = county))+	
    stat_smooth(method = "lm",  color="red")+	
    labs(x='log(bush2000)', y='log(buchanan2000)',	
         title = "Linear Model with Palm Beach")+	
    # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., ..adj.rr.label.., sep = "~~~")),	
    #              label.x.npc = "right", label.y.npc = 0.40, geom = "text",	
    #              formula =  y ~x, parse = TRUE, size = 3)+	
    # stat_fit_glance(method = "lm",	
    #                 method.args = list(formula = y~x),	
    #                 geom = "text", size = 3,	
    #                 label.y.npc = 0.25, label.x.npc = "right",	
    #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))+	
   # scale_x_log10() +	
   # scale_y_log10()+	
 	
    theme_classic()+ guides(fill=FALSE)+	
    theme(	
      axis.title = element_text( size=8),	
      axis.title.x = element_text( size=8),	
      axis.title.y = element_text( size=8)	
    )	
# Obtain a 95% prediction interval for the number of Buchanan votes in Palm Beach from this result-assuming the relationship is the same in this county as in the others.	
	
	
 grid.arrange(bush.withPalmBeach.plot,bush.withoutPalmBeach.plot , ncol = 1,	
               nrow = 2) 	
	
#' 	
#' 	
#' 	
#' 	
#' Using the regression model without palm beach to generate a 95% prediction interval for the number of Buchanan votes in the Palm Beach, we see that the expected votes for Buchanan  given the corresponding votes for Bush (152,846) is 592.377 which highly deviates from the observed votes of 3407	
#' 	
#' 	
	
bushPalmBeach.predict <- predict(bush.withoutPalmBeach.model,bushPalmBeach, interval = "prediction")	
	
stargazer(exp(bushPalmBeach.predict), title="Prediction for Buchanana",   type = "latex",	header=FALSE)	
#' 	
#' 	
#' 	
#' In fact, the observed votes for Buchanan (3407) is way far from our 95 % prediction interval of [250.8, 1399.164].  Therefore we have sufficient evidence that the Buchanan's actual count contains a number of votes intended for Gore because of the ballot paper candidate arrangement error. Using our prediction model, the extra votes for Buchanan (3407-592) was meant for Gore  hence decreasing Buchanan votes by 592 and increasing Gores votes from 268945 to 268353 	
#' 	
#' ## b. Analyze the data in Ex1222 and write a statistical summary predicting the number of Buchanan, votes in Palm Beach Country that were not intended for him. It would be appropriate to describe any unverifiable assumptions used in applying the prediction equation for this purpose. (Suggestion: Find a model for predicting Buchanan's 2000 vote from other variables, excluding Palm Beach County, which is listed last in the data set. Consider a transformation of all counts.)	
#' 	
#' We used Stepwise AIC for predictor variable selection and after evaluating the relative importance measure of each variable, we came up with the best model that is in line with the objective of this analysis.	
#' 	
#' ### Variable Selection	
#' 	
#' The full model containing all potential predictor variables were passed into the step function. The StepAIC function iteratively searched best predictors by dropping or adding one X predictor variable at a time. In each iteration,  AIC of the models were calculated and the model that had the lowest AIC was retained for the next iteration.	
#' 	
#' **Full Model: ** buchanan2000~bush2000+gore2000+nader2000+browne2000+total2000+clinton96+ dole96+perot96+buchanan96p+reform.reg+total.reg	
#' 	
#' After the step iterations were completed the full model was reduced to a model with	
#' 7 predictors: 	
#' 	
#' **StepWise AIC Model: **: buchanan2000 ~ nader2000 + browne2000 + total2000 + clinton96 + perot96 + buchanan96p + total.reg	
#' 	
#' The rule of thumb for a good predictive accuracy has always been to look at no more variables than 10% of the total number of observation. This model is in check with this rule, therefore, there is no need to remove any more predictors. In fact looking at these variables individually with relation to  2000 and 1996 elections we have much more relevancy in context compared to the other predictors	
#' 	
#' 	
	
  	
bush.withoutPalmBeach.allSubset.model <- lm(buchanan2000~bush2000+gore2000+nader2000+browne2000+total2000+clinton96+	
                                    dole96+perot96+buchanan96p+reform.reg+total.reg, data=bush.withoutPalmBeach)	
	
#plot(bush.withoutPalmBeach.stepAIC.model)	
#summary(bush.withoutPalmBeach.allSubset.model)	
#names(bush.withoutPalmBeach)	
	
bush.withoutPalmBeach.stepAIC  <- stepAIC(bush.withoutPalmBeach.allSubset.model, direction="both")	
bush.withoutPalmBeach.stepAIC.anova <- bush.withoutPalmBeach.stepAIC$anova	
	
	
	
# final model results	
bush.WPB.stepAIC.best.model <-lm(buchanan2000 ~ nader2000 + browne2000 + total2000 + clinton96 + 	
    perot96 + buchanan96p + total.reg, data=bush.withoutPalmBeach)	
	
	
#' 	
#' 	
#' 	
stargazer(bush.WPB.stepAIC.best.model,	
          header=F,		
          type = "latex",		
          summary = F,		
          no.space = T,		
          ci = TRUE,		
        # keep = c("\\bprecip\\b"),		
          title = "Best Model", 		
        	
         # notes = "Coefficients have been removed!",		
            dep.var.caption  = "-" ,  # Bold		
          single.row = T		
         		
          )		
#' 	
#' 	
#' This model has very high explanatory power, with R^2 of 0.898 and Adjusted R^2 of 0.886. Over 89.8% of the variability in Buchanan votes for 2000 is explained by selected predictors. Furthermore, most of the predictors have really low p-values except for browne2000	
#' 	
#' ### Relative Importance Measure	
#' 	
#' After building the model using stepwise variable selection, We went ahead and evaluated the relative importance of each predictor in the model.	
#' 	
#' 	
bush.WPB.relimp<- calc.relimp(bush.WPB.stepAIC.best.model,type=c("lmg"), rela=TRUE)	
	
# Bootstrap Measures of Relative Importance (1000 samples) 	
bush.WPB.stepAIC.best.boot <- boot.relimp(bush.WPB.stepAIC.best.model, b = 1000, type = c("lmg", 	
  "last", "first", "pratt"), rank = TRUE, 	
  diff = TRUE, rela = TRUE)	
#booteval.relimp(bush.WPB.stepAIC.best.boot) # print result	
plot(booteval.relimp(bush.WPB.stepAIC.best.boot,sort=TRUE)) # plot result	
	
#' 	
#' 	
#' Using Lindeman, Merenda, and Gold method, all the predictors selected from the stepWise regression had significant contribution percentage into the model, therefore, we did not have any reason to remove any of the variables identified by stepwiseAIC.	
#' 	
#' 	
#' 	
#' ### Prediction	
#' 	
#' 	
#' 	
	
bush.WPB.stepAIC.predicted <-predict(bush.WPB.stepAIC.best.model, 	
                                      bush.log[67,], 	
                                      se.fit=T, interval="pred")	
	
stargazer(exp(bush.WPB.stepAIC.predicted$fit), title="Best Prediction for Buchanana",   type = "latex",	header=FALSE)	
#' 	
#' 	
#' ### Assumptions	
#' 	
#' - Due to the fact we were modeling for predictive regression, we didn't assess multicollinearity	
#' - Relative importance is not as effective when regressors are correlated. We did not evaluate collinearity	
#' - We did not have a variable for total1996 and observations for other candidates in 1996. For us to standardize vote counts for the year 1996 and 2000, we needed these variables. Therefore we assumed that the data was standardized in the sense that the total number of votes for 1996 and 2000 were the same.	
#' 	
#' 	
#' We used the above model to predict Buchanan's 2000 vote in Palm Beach County. Among the candidate predictors include in this model were 2000 votes for Browne and Nader, 1996 votes for Dole, Clinton, Perot, and Buchanan. With a 95% prediction interval of about [164.101, 1136.736], we got an expected number of votes for Buchanan in Palm Beach County as 431.903 	
#' 	
#' The Observed number of votes for Buchanan was 3407 which was way higher than expected number of votes of 432. Furthermore, this observation is way far from our 95 % prediction interval of about [164,1137]. This can only mean that Buchanan's actual count contains a number of votes intended for Gore because of the ballot paper candidate arrangement error. Using our prediction model, part of the extra votes for Buchanan (3407 - 432) was meant for Gore. From the physical evidence presented and the statistical inference made, we have sufficient evidence that the expected number of votes for Gore was supposed to be approximately 271917 (268945+2972) instead of 268945. 	
#' 	
#' 	
#' 	
#' 	
#' 	
#' 	
#' \onecolumn	
#' 	
#' # Question 3	
#' 	
#' ## a. Compute "Jittered" versions of-treatment, days after inoculation, and an indicator variable for females by adding small random numbers to each (uniform random numbers between -.15 and .15 work well). Or you could use the jitter function.	
#' 	
#' 	
bloodBrain$brainLiver= bloodBrain$BRAIN/bloodBrain$LIVER	
bloodBrain=bloodBrain%>%	
  mutate(	
	
    isTreatJitter=jitter(ifelse(TREAT=='BD',1,0),amount = 0.15),	
    daysJitter=jitter(DAYS,amount = 0.15),	
    femaleJitter=jitter(ifelse(SEX=='F',1,0),amount = 0.15)	
    )	
	
bloodBrain$log_brainLiver= log(bloodBrain$brainLiver)	
bloodBrain$log_Time= log(bloodBrain$TIME)	
	
	
stargazer(bloodBrain[c(1,10:ncol(bloodBrain))],	
          header=F,	
          type = "latex",	
          no.space = T,	
          summary = F,	
          single.row = T	
          )	
	
#' 	
#' 	
#' ## b. Obtain a matrix of scatter plots for the following variables: log sacrifice time, treatment (jittered), days after inoculation (jittered), sex (jittered), and the log of the brain tumor-to-liver antibody ratio. Use the function pairs in the graphics package or scatterplotMatrix in the car package.	
#' 	
#' 	
#pairs(bloodBrain[11:15],pch=21)	
scatterplotMatrix(~log_Time+isTreatJitter+daysJitter+femaleJitter+log_brainLiver, data=bloodBrain,	
  	main="Variables Scatter Plot Matrix")	
#' 	
#' 	
#' ## c. Obtain a matrix of the correlation coefficients among the same five variables (not jittered).	
#' 	
#' 	
#library(ggcorrplot)	
	
#data(mtcars)	
#corr <- cor(bloodBrain[c(10,4,5,6,3)])	
# Second Correlogram Example	
# Correlations with significance levels	
#library(Hmisc)	
#mtcars is a data frame	
#rcorr(as.matrix(bloodBrain[c(10,4,5,6,3)]))	
  hist.panel = function (x, ...=NULL ) {	
	
        par(new = TRUE)	
	
        hist(x,	
	
             col = "light gray",	
	
             probability = TRUE,	
	
             axes = FALSE,	
	
             main = "",	
	
             breaks = "FD")	
	
        lines(density(x, na.rm=TRUE),	
	
              col = "red",	
	
              lwd = 1)	
	
        #lines(f, col="blue", lwd=1, lty=1) how to add gaussian normal overlay?	
	
        rug(x)	
	
  }	
	
corr.panel <- function(x, y, digits=2, prefix="", cex.cor) 	
{	
    usr <- par("usr"); on.exit(par(usr)) 	
    par(usr = c(0, 1, 0, 1)) 	
    r <- abs(cor(x, y)) 	
    txt <- format(c(r, 0.123456789), digits=digits)[1] 	
    txt <- paste(prefix, txt, sep="") 	
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 	
 	
    test <- cor.test(x,y) 	
    # borrowed from printCoefmat	
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 	
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),	
                  symbols = c("***", "**", "*", ".", " ")) 	
 	
    text(0.5, 0.5, txt, cex = 2) 	
    text(.8, .8, Signif, cex=1.5, col=2) 	
}	
	
  	
    	
	
pairs(bloodBrain[c(14,5,6,15,4)], lower.panel=corr.panel, upper.panel=corr.panel,  diag.panel=hist.panel)	
	
	
	
	
#' 	
#' 	
#' 	
#' ## d.	On the basis of this, what can be said about the relationship between the covariates (sex and days after inoculation), the response, and the design variables (treatment and sacrifice time.	
#' 	
#' 	
#' We have a very strong and very significant relationship between log response variable (ratio between antibodies in brain and liver) and the log of sacrifice time (r=.54). However, the relationship between the log of response and treatment is weak and insignificant. On the other hand, we have a moderately strong and significant relationship between log response variable and days after inoculation (r=.4). There is a strong and significant relationship between log response and sex (r=.56). We do not have a correlation between the covariates (sex and days after inoculation), their relationship is weak and insignificant. Also, the relationship between days after inoculation and log of sacrifice time is weak (r=.33) with a marginal p-value. We also have a weak insignificant signal between Days after inoculation and treatment. On the other hand, the relationship between sex and log of sacrifice time was moderately strong and very significant (r=.54). However, we do not have any signal between sex and treatment (r=0). Finally, there is no correlation between the design variable ( treatment and log of sacrifice time)	
#' 	
#' In summary, we don't have collinearity within the design variables (treatment and log of sacrifice time) as well as within the covariates (sex and days after inoculation). However, we do have collinearity between sex and log of sacrifice time. The relationship between the log response variable and all the predictors are mostly strong and significant except for the treatment predictor variable.	
#' 	
#' \onecolumn	
#' 	
#' ## e.	Fit the regression of the log response (brain tumor-to-liver antibody ratio) on an indicator variable for treatment and on sacrifice time treated as a factor with four levels (include three indicator variables, for sacrifice time == 3, 24, and 72 hours). Use the model to find the estimated mean of the log response at each of the eight treatment combinations (all combinations of the two infusions and the four sacrifice times).	
#' 	
#' 	
	
bloodBrain$TIMEFactor<-as.factor(bloodBrain$TIME)	
bloodBrain.model <- lm(log_brainLiver~TIMEFactor+TREAT,data=bloodBrain)	
	
stargazer(bloodBrain.model,	
          header=F,		
          type = "latex",		
          summary = F,		
          no.space = T,		
          ci = TRUE,		
        # keep = c("\\bprecip\\b"),		
          title = "", 		
        	
         # notes = "Coefficients have been removed!",		
            dep.var.caption  = "-" ,  # Bold		
          single.row = T		
         		
          )		
	
	
bloodBrain.treatment <- data.frame(	
  TIMEFactor=as.factor(rep(c(.5,3,24,72),2)),	
   TREAT=as.factor(c(rep("BD",4),rep("NS",4)))	
)	
	
bloodBrain.model.predicted <- cbind(bloodBrain.treatment,	
data.frame(predict(bloodBrain.model,bloodBrain.treatment, interval = "prediction")))	
	
stargazer(bloodBrain.model.predicted,	
          header=F,	
          type = "latex",	
          no.space = T,	
          summary = F,	
          single.row = T	
          )	
	
#' 	
#' \onecolumn	
#' 	
#' ## f.	Let X represent log of sacrifice time. Fit the regression of the log response on an indicator variable for treatment, X, X2, and X3. Use the estimated model to find the estimated mean of the log response at each of the eight treatment combinations.	
#' 	
#' 	
bloodBrain.model.2 <- lm(log_brainLiver~TREAT+log_Time+log_Time^2+log_Time^3,data=bloodBrain)	
	
stargazer(bloodBrain.model.2,	
          header=F,		
          type = "latex",		
          summary = F,		
          no.space = T,		
          ci = TRUE,		
        # keep = c("\\bprecip\\b"),		
          title = "", 		
        	
         # notes = "Coefficients have been removed!",		
            dep.var.caption  = "-" ,  # Bold		
          single.row = T		
         		
          )		
	
	
bloodBrain.treatment.2 <- data.frame(	
  log_Time=log(rep(c(.5,3,24,72),2)),	
   TREAT=as.factor(c(rep("BD",4),rep("NS",4)))	
)	
	
bloodBrain.model.2.predicted <- cbind(bloodBrain.treatment.2,	
data.frame(predict(bloodBrain.model.2,bloodBrain.treatment.2, interval = "prediction")))	
	
	
	
stargazer(bloodBrain.model.2.predicted,	
          header=F,	
          type = "latex",	
          no.space = T,	
          summary = F,	
          single.row = T	
          )	
	
	
#' 	
#' 	
#' ## g.	Why are the answers to parts (5) and (6) the same?	
#' 	
#' 	
augment(bloodBrain.model.2, bloodBrain) %>%	
  ggscatter(	
  x = ".fitted",	
  y = "log_brainLiver",	
  conf.int = T,	
  cor.coef = T,	
  cor.method = "pearson",	
  add = "reg.line"	
  	
  )	
	
	
augment(bloodBrain.model, bloodBrain) %>%	
  ggscatter(	
  x = ".fitted",	
  y = "log_brainLiver",	
  conf.int = T,	
  cor.coef = T,	
  cor.method = "pearson",	
  add = "reg.line"	
  	
  )	
	
#' 	
#' 	
#' 	
#' 	
bloodBrain.model.anova <- anova(bloodBrain.model, bloodBrain.model.2)	
stargazer(bloodBrain.model.anova,	
          header=F,	
          type = "latex",	
          summary = F,	
          no.space = T,	
          single.row = T	
          )	
# there is no significant difference between the 4 models beccause all the p-values are extremly high	
#' 	
#' 	
#' Since the origin model is already linear, further log transformation wouldn't change any of the predictions by much. Transformation doesn't have much effect if the data points don't vary too much. In fact, plotting out a scatter plot for data of the 2 models indicates that the range of data points don't vary and spread a lot as we would have expected in a classical scenario where variable transformation was necessary.  	
#' 	
#' ## h.	Fit the regression of the log response (brain tumor-to-liver antibody ratio) on all covariates, the treatment indicator, and sacrifice time, treated as a factor with four levels (include three indicator variables, for sacrifice time == 3, 24, and 72 hours).	
#' 	
#' 	
bloodBrain.model.3 = lm(log_brainLiver~ TREAT+ TIMEFactor+ SEX+ WEIGHT+ DAYS+ LOSS + TUMOR, data=bloodBrain)	
bloodBrain.model.3.df=augment(bloodBrain.model.3,bloodBrain)	
#model4.df	
stargazer(bloodBrain.model.3,	
          header=F,		
          type = "latex",		
          summary = F,		
          no.space = T,		
          ci = TRUE,		
        # keep = c("\\bprecip\\b"),		
          title = "", 		
        	
         # notes = "Coefficients have been removed!",		
            dep.var.caption  = "-" ,  # Bold		
          single.row = T		
         		
          )		
#' 	
#' 	
#' ## i.	Obtain a set of case influence statistics, including a measure of influence, the leverage, and the studentized residua1.	
#' 	
#' 	
# influencial points	
bloodBrain.model.3.influence <- influence.measures(bloodBrain.model.3)	
bloodBrain.model.3.influence.df <- data.frame(bloodBrain.model.3.influence[[1]])	
	
# studentized 	
bloodBrain.model.3.studres <- studres(bloodBrain.model.3)	
	
bloodBrain.model.3.influence.df$studResidual<-bloodBrain.model.3.studres	
	
 as_tibble(bloodBrain.model.3.influence.df)%>%	
  mutate_if(is.numeric, funs(round(., 5))) %>% 	
     dplyr::select(dffit,cov.r,cook.d,hat,studResidual)%>%	
        stargazer(	
          header=F,	
          type = "latex",	
          summary = F,	
          no.space = T,	
          single.row = T	
          )	
#' 	
#' 	
#' \twocolumn	
#' 	
#' ### Cook’s Distance 	
#' 	
#' 	
	
ols_cooksd_barplot(bloodBrain.model.3)	
	
#' 	
#' 	
#' 	
#' ### DFFITS Plot  - difference in fits	
#' 	
#' 	
	
	
ols_dffits_plot(bloodBrain.model.3)	
	
	
#' 	
#' 	
#' 	
#'  	
#' ### Studentized Residual Plot	
#' 	
#' 	
	
ols_srsd_plot(bloodBrain.model.3)	
	
#' 	
#' 	
#' 	
#' ### Leverage Plot	
#' 	
#' 	
	
ols_rsdlev_plot(bloodBrain.model.3)	
	
#' 	
#' 	
#'  	
#' ### Deleted Stud.Residual vs Fitted Values	
#' 	
#' 	
	
ols_dsrvsp_plot(bloodBrain.model.3)	
	
#' 	
#' 	
#' 	
#' 	
#' ### Hadi Plot	
#' 	
#' 	
	
ols_hadi_plot(bloodBrain.model.3)	
	
#' 	
#' 	
#' 	
#' \onecolumn	
#' 	
#' ## * Discuss whether any influential observations or outliers occur with respect to this fit.	
#' 	
#' - The cook's distance plot revealed that we have 3 regression outliers which strongly influences fitted values of our model. These outliers include observation 31, 33, and 34. 	
#' 	
#' - DFFIT diagnostics plot indicated that we have 6 influential data points. Observation 31, 33 and 34 had the strongest influence while observation 27, 22 and 8 had marginal (acceptable) influence on our model	
#' 	
#' - From the Studentized residual plot, we found out that 1 observation which had studentized residual greater than 3 (absolute cutoff) indicating that observation 34 is an outlier that has a large effect on the overall residual	
#' 	
#' - Leverage plot indicated that we did not have any leverage observations since we did not have any observation with an extreme value on a predictor variable	
#' 	
#' - Deleted Studentized Residual vs Fitted Value plot indicated that we had 2 observations which pulled the estimated regression line towards themselves. This implies that if observation 31 and 34 are deleted from the model then the overall observed response would be much closer to the predicted response.	
#' 	
#' 	
#' In summary, two observations had the highest influence on our regression model implying that if included in the model, they would substantially change the estimate of our coefficients. Observation 31 had a cook distance of 0.3113, hat value of 0.38319, dffit of 1.94181 and studentized residual of 2.46364. Observation 34  had a cook distance of 0.4772, hat value of 0.28905, dffit of -2.9917 and studentized residual of -4.69189	
#' 	
#' \onecolumn	
#' 	
#' # Question 4	
#' 	
#' ![](pic/1.png)	
#' 	
#' ![](pic/2.png)	
#' 	
#' 	
#' ![](pic/3.png)	
#' 	
#' \onecolumn	
#' 	
#' # Question 5	
#' 	
#' ## LMQ Function	
#' 	
#' 	
	
library(stats4)	
library(dplyr)	
	
lmq<-function(x,y){	
  	
  set.seed(120) #reproducability	
  	
  beta0=1 # init starting point	
  beta1=1 # init starting point	
  sigma=1 # init starting point	
  q=1 # init starting point	
 	
  method = "L-BFGS-B" # or BFGS	
  	
  # par(mfrow = c(1,1))		
  # plot(x,y)	
  	
  logLikelihood <- function(beta0,beta1,sigma,q){	
    beta= t(c(beta0,beta1)) # transpose and convert to matrix	
    resid = y - beta0 - beta1*x	
    R = dnorm(resid, mean = 0, sd=sqrt(x^(q)*sigma^2), log=TRUE)	
    -sum(R)	
  }	
  	
  mle(logLikelihood, list(beta0=beta0, beta1=beta1, sigma=sigma, q=q), method =method)	
}	
#' 	
#' 	
#' 	
#' ## Test Run 1: Simulate data with beta0=1.4, beta1=1.8 , sigma=2, and q=1	
#' 	
#' 	
	
set.seed(120) #reproducability	
# test run1	
x = rep(seq(1,3,by=0.5),2000)	
y = 1.4 + 1.8*x + rnorm(length(x),0,2*x^(1/2))	
plot(x,y)	
lmq(x,y)	
	
	
	
#' 	
#' 	
#' ## Test Run 1: Simulate data with beta0=6, beta1=3 , sigma=5, and q=2	
#' 	
#' 	
	
set.seed(120) #reproducability	
	
# test run2	
x = rep(seq(1,3,by=0.5),2000)	
y = 6 + 3*x + rnorm(length(x),0,5*x^(2/2))	
plot(x,y)	
lmq(x,y)	
	
	
	
	
#' 	
#' 	
#' \onecolumn	
#' 	
#' # Question 6	
#' 	
#' 	
#' ![](pic/4.png)	
#' 	
#' 	
#' 	
#' ![](pic/5.png)	
#' 	
#' ![](pic/6.png)	
#' 	
#' \onecolumn	
#' 	
#' # Source Code	
#' 	
#' 	
	
	
#' 	
